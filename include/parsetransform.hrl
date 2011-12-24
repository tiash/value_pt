-define(forms(F), erl_syntax:flatten_form_list(erl_syntax:form_list(lists:flatten([F])))).
-define(unforms(F), erl_syntax:form_list_elements(?forms(F))).

-compile([{nowarn_unused_function,syntax_fold/2}]).
-compile([{nowarn_unused_function,syntax_fold/3}]).
-compile([{nowarn_unused_function,syntax_fold/4}]).
-compile([{nowarn_unused_function,syntax_fold_inner/4}]).
-compile([{nowarn_unused_function,analyze_attribute/1}]).
-compile([{nowarn_unused_function,concrete_attribute/1}]).
-compile([{nowarn_unused_function,insert_at_top/2}]).
-compile([{nowarn_unused_function,insert_in_head/2}]).
-compile([{nowarn_unused_function,insert_at_end/2}]).

-define(atom(Atom),erl_syntax:atom(Atom)).
-define(var(Var),erl_syntax:variable(Var)).
-define(var(Prefix,Name),?var(list_to_atom(atom_to_list(Prefix)++"_"++atom_to_list(Name)))).
-define(apply(Fun,Args),erl_syntax:application(?atom(Fun),Args)).
-define(apply(Mod,Fun,Args),erl_syntax:application(?atom(Mod),?atom(Fun),Args)).
-define(clause(Pattern,Guard,Body),erl_syntax:clause(Pattern,Guard,Body)).
-define(cases(Arg,Clauses),erl_syntax:case_expr(Arg,Clauses)).
-define(ifs(Clauses),erl_syntax:if_expr(Clauses)).
-define(record(Value,Name,Fields),erl_syntax:record_expr(Value,?atom(Name),Fields)).
-define(record(Name,Fields),erl_syntax:record_expr(?atom(Name),Fields)).
-define(field(Name),erl_syntax:record_field(?atom(Name))).
-define(field(Name,Value),erl_syntax:record_field(?atom(Name),Value)).
-define(int(Val),erl_syntax:integer(Val)).
-define(underscore,erl_syntax:underscore()).
-define(tuple(Elems),erl_syntax:tuple(Elems)).
-define(function(Name,Clauses),erl_syntax:function(?atom(Name),Clauses)).
-define(match(Left,Right),erl_syntax:match_expr(Left,Right)).
-define(abstract(Term),erl_syntax:abstract(Term)).
-define(cons(Head,Tail),erl_syntax:cons(Head,Tail)).
-define(list(Elems),erl_syntax:list(Elems)).
-define(list(Elems,Tail),erl_syntax:list(Elems,Tail)).
-define(nil,erl_syntax:nil()).
-define(func(Clauses),erl_syntax:fun_expr(Clauses)).
-define(access(Value,Record,Field),erl_syntax:record_access(Value,?atom(Record),?atom(Field))).
-define(infix(A,B,C),erl_syntax:infix_expr(A,erl_syntax:operator(B),C)).
-define(eq(A,B),?infix(A,'==',B)).
-define(eeq(A,B),?infix(A,'=:=',B)).
-define(neq(A,B),?infix(A,'=/=',B)).
-define(gt(A,B),?infix(A,'>',B)).
-define(gteq(A,B),?infix(A,'>=',B)).
-define(lt(A,B),?infix(A,'<',B)).
-define(lteq(A,B),?infix(A,'=<',B)).

-define(suppress_unused(Fun,Arity),erl_syntax:attribute(?atom(compile),[ ?abstract({nowarn_unused_function,{Fun,Arity}}) ])).

syntax_fold(Fun,Expr) ->
  {R1,_,_} =
    syntax_fold(fun (E,B,C) ->
      case Fun(E) of
        skip -> skip;
        done -> done;
        {done,NE} -> {done, NE,B,C};
        NE -> {NE,B,C}
      end end, Expr, undefined, []),
  R1.
syntax_fold(Fun,Expr,State) ->
  {R1,R2,_} =
    syntax_fold(fun (E,S,C) ->
      case Fun(E,S) of
        skip -> skip;
        done -> done;
        {skip,NS} -> {skip,NS,C};
        {done,NS} -> {done,NS,C};
        {done,NE,NS} -> {done,NE,NS,C};
        {NE,NS} -> {NE,NS,C}
      end end, Expr, State, []),
  {R1,R2}.
syntax_fold(_Fun,none,State,Stack) -> {none,State,Stack};
syntax_fold(_Fun,[],State,Stack) -> {[],State,Stack};
syntax_fold(Fun,[E|Es],State,Stack) ->
  {E2,State2,Stack2} = syntax_fold(Fun,E,State,Stack),
  {Es2,State3,Stack3} = syntax_fold(Fun,Es,State2,Stack2),
  {[E2|Es2],State3,Stack3};
syntax_fold(Fun,Expr,State,Stack) ->
  case Fun(Expr,State,Stack) of
    skip -> syntax_fold_inner(Fun, Expr, State, Stack);
    {skip,NState,NStack} -> syntax_fold_inner(Fun, Expr, NState, NStack);
    done -> {Expr,State,Stack};
    {done,NState,NStack} -> {Expr,NState,NStack};
    {done,NExpr,NState,NStack} -> {NExpr,NState,NStack};
    {NExpr,NState,NStack} -> syntax_fold_inner(Fun,NExpr,NState,NStack)
  end.
syntax_fold_inner(Fun,Expr,State,Stack) ->
  Data = erl_syntax:subtrees(Expr),
  case syntax_fold(Fun,Data,State,[ erl_syntax:type(Expr) | Stack]) of
    {Data,State,[_|Stack]} -> {Expr,State,Stack};
    {NData,NState,[_|NStack]} -> {erl_syntax:update_tree(Expr,NData),NState,NStack}
  end.

analyze_attribute(Form) ->
  case erl_syntax:type(Form) of
    attribute ->
      Name = erl_syntax:atom_value(erl_syntax:attribute_name(Form)),
      case erl_syntax:attribute_arguments(Form) of
        none -> Name;
        Args -> list_to_tuple([Name | [ concrete_attribute(A) || A<-Args ] ])
      end;
    _ -> false
  end.
concrete_attribute(Expr) ->
  case erl_syntax:type(Expr) of
    atom -> erl_syntax:atom_value(Expr);
    arity_qualifier -> { concrete_attribute(erl_syntax:arity_qualifier_body(Expr)), concrete_attribute(erl_syntax:arity_qualifier_argument(Expr))};
    integer -> erl_syntax:integer_value(Expr);
    list -> [concrete_attribute(erl_syntax:list_head(Expr)) | concrete_attribute(erl_syntax:list_tail(Expr)) ];
    nil -> [];
    string -> erl_syntax:string_value(Expr);
    tuple -> list_to_tuple([ concrete_attribute(E) || E<-erl_syntax:tuple_elements(Expr)]);
    float -> erl_syntax:float_value(Expr);
    _ -> '#undefined#'
  end.


insert_at_top(Syntax,Insert) ->
  Forms = ?unforms(Syntax),
  {Top,Body} = lists:splitwith(fun (F) -> case erl_syntax:type(F) of attribute ->
        case erl_syntax_lib:analyze_attribute(F) of
          {module,_} -> true;
          {file,_} -> true;
          _ -> false
        end; eof_marker -> false; _ -> false  end end, Forms),
  ?forms([Top,Insert,Body]).

insert_in_head(Syntax,Insert) ->
  Forms = ?unforms(Syntax),
  {Head,Body} = lists:splitwith(fun (F) -> case erl_syntax:type(F) of function -> false; eof_marker -> false; _ -> true  end end, Forms),
  ?forms([Head,Insert,Body]).
  
insert_at_end(Syntax,Insert) ->
  Forms = ?unforms(Syntax),
  {Body,EOF} = lists:splitwith(fun (F) -> case erl_syntax:type(F) of eof_marker -> false; _ -> true end end, Forms),
  ?forms([Body,Insert,EOF]).

    




-module(value_pt).

-export([parse_transform/2]).

-undef(DEBUG).
% -define(DEBUG,true).
-include("debug.hrl").

-include("parsetransform.hrl").

parse_transform(Forms0,_Options) ->
  Forms1 = ?forms(Forms0),
  Module = getModule(Forms1),
  Fields = getFields(Forms1),
  Forms2 = insert_in_head(Forms1, fieldMembers(Module,Fields)),
  Members = getMembers(Forms2),
  Forms3 = insert_at_top(Forms2, module_record(Module,Fields)),
  Forms4 = insert_at_end(Forms3, initLogic(Module,Fields)),
  Forms5 = insert_at_end(Forms4, fieldLogic(Module,Fields)),
  Forms6 = insert_in_head(Forms5, memberExports(Module,Members,Fields)),
  Forms7 = mangleMembers(Members,Forms6),
  ?debug_pt(Forms7),
  erl_syntax:revert_forms(Forms7).

getModule(Forms) ->
  case erl_syntax_lib:fold(
        fun (E,Accum) ->
          case erl_syntax:type(E) of
            attribute ->
              case erl_syntax_lib:analyze_attribute(E) of
                {module,Module} -> Accum++[Module];
                _ -> Accum
              end;
            _ -> Accum
          end
        end,[],Forms) of
    [Module] -> Module
  end.

getFields(Forms) ->
  erl_syntax_lib:fold(
        fun (E,Accum) ->
          case erl_syntax:type(E) of
            attribute ->
              case erl_syntax_lib:analyze_attribute(E) of
                {field,{field,Field}} -> Accum++[field(Field)];
                _ -> Accum
              end;
            _ -> Accum
          end
        end,[],Forms).

-record(ifdef, { value = undefined :: term(), update = replace :: update() }).
-type update() :: immutable | repace | update | #ifdef{} | atom() | {module(),atom()}.
-record(default, {value :: term()}).
-record(field,  { name :: atom()
                , getter :: atom()
                , getter_public = false :: boolean()
                , setter :: atom()
                , setter_public = false :: boolean()
                , default = no_default :: no_default | #default{}
                , update = #ifdef{update=replace} :: update()
                }).
field({Name,Opts}) when is_atom(Name), is_list(Opts) ->
  field_opts(#field{name=Name,getter=Name,setter=Name},Opts);
field(Name) when is_atom(Name) -> field({Name,[]});
field(Field=#field{}) -> Field.
field_opts(Field,[{public,State}|Opts]) -> field_opts(Field,[{getter_public,State},{setter_public,State} | Opts]);
field_opts(Field,[{getter_public,State}|Opts]) -> field_opts(Field#field{getter_public=State},Opts);
field_opts(Field,[{setter_public,State}|Opts]) -> field_opts(Field#field{setter_public=State},Opts);
field_opts(Field,[{private,State}|Opts]) -> field_opts(Field,[{public,not(State)} | Opts]);
field_opts(Field,[{getter_private,State}|Opts]) -> field_opts(Field,[{getter_public,not(State)} | Opts]);
field_opts(Field,[{setter_private,State}|Opts]) -> field_opts(Field,[{setter_public,not(State)} | Opts]);
field_opts(Field,[{getter,Fun}|Opts]) -> field_opts(Field#field{getter=Fun},Opts);
field_opts(Field,[{setter,Fun}|Opts]) -> field_opts(Field#field{setter=Fun},Opts);
field_opts(Field,[{no_default,true}|Opts]) -> field_opts(Field#field{default=no_default},Opts);
field_opts(Field,[{default,Value}|Opts]) ->
  case Field#field.default of
    Default=#default{} -> field_opts(Field#field{default=Default#default{value=Value}},Opts);
    no_default -> field_opts(Field#field{default=#default{value=Value}},Opts)
  end;
field_opts(Field,[{immutable,true}|Opts]) -> field_opts(Field,[{update,immutable},setter_private,{setter,undefined}|Opts]);
field_opts(Field,[{update,Update}|Opts]) -> field_opts(Field#field{update=Update},Opts);
field_opts(Field,[Key | Opts]) when is_atom(Key) -> field_opts(Field,[{Key,true} | Opts]);
field_opts(Field,[_|Opts]) -> field_opts(Field,Opts);
field_opts(Field,[]) -> Field.

getMembers(Forms) ->
  erl_syntax_lib:fold(
        fun (E,Accum) ->
          case erl_syntax:type(E) of
            attribute ->
              case erl_syntax_lib:analyze_attribute(E) of
                {member,{member,Member}} -> Accum++[member(Member)];
                _ -> Accum
              end;
            _ -> Accum
          end
        end,[],Forms).

-record(member,{name :: atom(), arity :: pos_integer(), public = false :: boolean(), mutator = false :: boolean(), mangle = true :: boolean() }).
member({{Name,Arity},Opts}) when is_atom(Name), is_integer(Arity), is_list(Opts) ->
  member_opts(#member{name=Name,arity=Arity},Opts);
member({Name,Arity}) when is_atom(Name), is_integer(Arity) -> member({{Name,Arity},[]});
member(Member=#member{}) -> Member.
member_opts(Member,[{public,State}|Opts]) -> member_opts(Member#member{public=State},Opts);
member_opts(Member,[{private,State}|Opts]) -> member_opts(Member,[{public,not(State)} | Opts]);
member_opts(Member,[{mutator,State}|Opts]) -> member_opts(Member#member{mutator=State},Opts);
member_opts(Member,[{accessor,State}|Opts]) -> member_opts(Member,[{mutator,not(State)} | Opts]);
member_opts(Member,[{mangle,State}|Opts]) -> member_opts(Member#member{mangle=State},Opts);
member_opts(Member,[{no_mangle,State}|Opts]) -> member_opts(Member,[{no_mangle,not(State)} | Opts]);
member_opts(Member,[Key | Opts]) when is_atom(Key) -> member_opts(Member,[{Key,true} | Opts]);
member_opts(Member,[_|Opts]) -> member_opts(Member,Opts);
member_opts(Member,[]) -> Member.

module_record(Module,Fields) ->
  erl_syntax:attribute(?atom(record),[?atom(Module), ?tuple(
    [ ?field(F#field.name,
        case F#field.default of
          no_default -> ?atom('#UNDEF#');
          #default{value=Val} -> ?abstract(Val)
        end)
    || F<-Fields ])]).
mangleMembers(Members,Forms) ->
  erl_syntax_lib:map_subtrees(
      fun (Form) ->
        case erl_syntax:type(Form) of
          function ->
            Function = erl_syntax:function_name(Form),
            Name =  erl_syntax:atom_value(Function),
            Arity = erl_syntax:function_arity(Form),
            Clauses = erl_syntax:function_clauses(Form),
            case findMember(Name,Arity,Members) of
              #member{mangle=true,mutator=Mutator} ->
                erl_syntax:function(Function, [ mangleMemberClause(Mutator,C,Members) || C <- Clauses ]);
              _ -> Form
            end;
          _ -> Form
        end
      end,Forms).

newVar(Prefix,Vars0) when is_atom(Prefix) -> newVar(atom_to_list(Prefix),Vars0);
newVar(Prefix,Vars0) ->
  Var1 = erl_syntax_lib:new_variable_name(fun (N) -> list_to_atom(Prefix++integer_to_list(N)) end,Vars0),
  Vars1 = sets:add_element(Var1,Vars0),
  {?var(Var1),Vars1}.
newVar(Vars0) -> newVar("_VAR_",Vars0).
newSelf(Vars0) -> newVar("_SELF_",Vars0).



mangleMemberClause(Mutator,Clause,Members) -> 
  Pattern = erl_syntax:clause_patterns(Clause) ++ [erl_syntax:variable('SELF')],
  Guard = erl_syntax:clause_guard(Clause),
  Body = erl_syntax:clause_body(Clause),
  {NewClause,_} = mangleMemberExpr( Members
                                    , case Mutator of true -> mutator; _ -> true end
                                    , erl_syntax:clause(Pattern,Guard,Body)
                                    , newSelf(erl_syntax_lib:variables(Clause))),
  NewClause.

mangleMemberExprFold(Members,Tail,Expr,State1) ->
  {Expr2,State2={Self2,_}} = erl_syntax_lib:mapfold_subtrees(fun (E,S) -> mangleMemberExpr(Members,false,E,S) end,State1,Expr),
  case Tail of
    mutator -> {erl_syntax:block_expr([Expr2,Self2]),State2};
    _ -> {Expr2,State2}
  end.

mangleMemberExpr(_Members,_Tail,none,State) -> {none,State};
mangleMemberExpr(_Members,_Tail,[],State) -> {[],State};
mangleMemberExpr(Members,Tail,[Expr],State1) when Expr=/=none -> 
  {NExpr,State2} = mangleMemberExpr(Members,Tail,Expr,State1),
  {[NExpr],State2};
mangleMemberExpr(Members,Tail,[Expr|Exprs],State1) -> 
  {NExpr,State2} = mangleMemberExpr(Members,false,Expr,State1),
  {NExprs,State3} = mangleMemberExpr(Members,Tail,Exprs,State2),
  {[NExpr|NExprs],State3};
mangleMemberExpr(Members,Tail,Expr,State1={Self1,Vars1}) ->
  case erl_syntax:type(Expr) of
    variable -> case erl_syntax:variable_name(Expr) of 'SELF' -> {Self1,State1}; _ -> {Expr,State1} end;
    application ->
      Operator = erl_syntax:application_operator(Expr),
      Arguments = erl_syntax:application_arguments(Expr),
      case erl_syntax:type(Operator) of
        atom ->
          FunName = erl_syntax:atom_value(Operator),
          FunArity = length(Arguments),
          case findMember(FunName,FunArity,Members) of
            Fun = #member{} ->
              {NApply,State2={_,Vars2}} = mangleMemberExprFold(Members,false,erl_syntax:application(Operator,Arguments++[?var('SELF')]),State1),
              case Fun#member.mutator of
                true ->
                  case Tail of
                    false ->
                      State3={Self3,_} = newSelf(Vars2),
                      {erl_syntax:match_expr(Self3,NApply),State3};
                    _ -> {NApply,State2}
                  end;
                _ -> {NApply,State2}
              end;
            _ -> mangleMemberExprFold(Members,Tail,Expr,State1)
          end;
        _ -> mangleMemberExprFold(Members,Tail,Expr,State1)
      end;
    case_expr ->
      {Arg,State2} = mangleMemberExpr(Members,false,erl_syntax:case_expr_argument(Expr),State1),
      {Cases,State3} = mangleMemberExprsClauses(Members,Tail,erl_syntax:case_expr_clauses(Expr),State2),
      {erl_syntax:case_expr(Arg,Cases),State3};
    if_expr ->
      {Cases,State2} = mangleMemberExprsClauses(Members,Tail,erl_syntax:if_expr_clauses(Expr),State1),
      {erl_syntax:if_expr(Cases),State2};
    receive_expr ->
      {[TimeOut|Cases],State2} = mangleMemberExprsClauses(Members,Tail,[?clause([erl_syntax:receive_expr_timeout(Expr)],none,erl_syntax:receive_expr_action(Expr))|erl_syntax:receive_expr_clauses(Expr)],State1),
      {erl_syntax:receive_expr(Cases,hd(erl_syntax:clause_patterns(TimeOut)),erl_syntax:clause_body(TimeOut)),State2};
    try_expr ->
      Body0 = erl_syntax:try_expr_body(Expr),
      Cases0 = erl_syntax:try_expr_clauses(Expr),
      Handlers0 = erl_syntax:try_expr_handlers(Expr),
      After0 = erl_syntax:try_expr_after(Expr),
      case Cases0 of
        [] = Cases2 ->
          {Handlers2,{Self2,Vars2}} = mangleMemberExprsClauses(Members,Tail,Handlers0,State1),
          {Body1,{Self3,Vars3}} = mangleMemberExpr(Members,Tail,Body0,{Self1,Vars2}),
          {Body2,{SelfX,VarsX}} = mangleMemberExprsEnsureSelf(Tail,Body1,Self3,{Self2,Vars3});
        _ ->
          {Body2,{Self2,Vars2}} = mangleMemberExpr(Members,false,Body0,State1),
          {Cases1,{_,Vars3}} = mangleMemberExprsClauses1(Members,Tail,Cases0,{Self2,Vars2}),
          {Handlers1,State4} = mangleMemberExprsClauses1(Members,Tail,Handlers0,{Self1,Vars3}),
          {HandlersCases,{SelfX,VarsX}} = mangleMemberExprsClauses2(Tail,Cases1++Handlers1,State4),
          {Cases2,Handlers2} = lists:split(length(Cases1),HandlersCases)
      end,
      {After2,{_,VarsY}} = mangleMemberExpr(Members,true,After0,{Self1,VarsX}),
      {erl_syntax:try_expr(Body2,Cases2,Handlers2,After2),{SelfX,VarsY}};
    implicit_fun -> {Expr,State1}; %% Functions can not manipulate 'SELF'...
    clause ->
      {Pattern,State2} = mangleMemberExpr(Members,false,erl_syntax:clause_patterns(Expr),State1),
      {Guard,State3} = mangleMemberExpr(Members,false,erl_syntax:clause_guard(Expr),State2),
      {Body,State4} = mangleMemberExpr(Members,Tail,erl_syntax:clause_body(Expr),State3),
      {erl_syntax:clause(Pattern,Guard,Body),State4};
    match_expr -> 
      Left = erl_syntax:match_expr_pattern(Expr),
      Right = erl_syntax:match_expr_body(Expr),
      case erl_syntax:type(Left) of
        variable ->
          case erl_syntax:variable_name(Left) of
            'SELF' ->
              case Tail of
                false ->
                  {Self2,Vars2} = newSelf(Vars1),
                  {NExpr,{_,Vars3}} = mangleMemberExpr(Members,false,Right,{Self1,Vars2}),
                  {erl_syntax:match_expr(Self2,NExpr),{Self2,Vars3}};
                _ ->
                  mangleMemberExpr(Members,Tail,Right,State1)
              end;
            _ -> mangleMemberExprFold(Members,Tail,Expr,State1)
          end;
        _ -> mangleMemberExprFold(Members,Tail,Expr,State1)
      end;
    _ -> mangleMemberExprFold(Members,Tail,Expr,State1)
  end.

mangleMemberExprsClauses(Members,Tail,OCases,State1) ->
  {Cases,State2} = mangleMemberExprsClauses1(Members,Tail,OCases,State1),
  mangleMemberExprsClauses2(Tail,Cases,State2).

mangleMemberExprsClauses1(_Members,_Tail,[],State1) -> {[],State1};
mangleMemberExprsClauses1(Members,Tail,OCases,{Self1,Vars1}) ->
  {Cases=[{_,Self2}|_],Vars2} =
      lists:mapfoldl(fun (C1,VarsN) ->
        {C2,{SelfM,VarsM}} = mangleMemberExpr(Members,Tail,C1,{Self1,VarsN}),
        {{C2,SelfM},VarsM}
      end, Vars1, OCases),
  {Cases,{Self2,Vars2}}.
mangleMemberExprsClauses2(Tail,Cases,{Self1,Vars1}) ->
  case lists:all(fun ({_,T}) -> T==Self1 end,Cases) of
    true -> {[ C || {C,_} <- Cases ],{Self1,Vars1}};
    _ ->
      {Self2,Vars2} = newSelf(Vars1),
      {NCases,Vars3} = lists:mapfoldl(fun ({Clause,OSelf},VarsN) ->
            ?debug(Clause),
            {Body,{_,VarsM}} = ?debug(mangleMemberExprsEnsureSelf(?debug(Tail),?debug(erl_syntax:clause_body(Clause)),?debug(OSelf),?debug({Self2,VarsN}))),
            {erl_syntax:clause(erl_syntax:clause_patterns(Clause),erl_syntax:clause_guard(Clause),Body),VarsM}
          end,Vars2,Cases),
      {NCases,{Self2,Vars3}}
  end.

  
mangleMemberExprsEnsureSelf(Tail,Exprs,OSelf,State1={NSelf,_}) ->
  {Before,[Last]} = lists:split(length(Exprs)-1,Exprs),
  case erl_syntax:type(Last) of
    match_expr ->
      case erl_syntax:match_expr_pattern(Last) of
        OSelf -> {Before++[?match(NSelf,erl_syntax:match_expr_body(Last))], State1};
        _ -> mangleMemberExprsEnsureSelf_(Tail,Before,Last,OSelf,State1)
      end;
    _ -> mangleMemberExprsEnsureSelf_(Tail,Before,Last,OSelf,State1)
  end.
mangleMemberExprsEnsureSelf_(false,Before,Last,OSelf,{NSelf,Vars1}) ->
  {Var,Vars2} = newVar(Vars1),
  {Before ++ [?match(Var,Last),?match(NSelf,OSelf),Var],{NSelf,Vars2}};
mangleMemberExprsEnsureSelf_(_Tail,Before,Last,_OSelf,State1={_,_}) ->
  {Before ++ [Last],State1}.

      
  
  

findMember(_Name,_Arity,[]) -> undefined;
findMember(Name,Arity,[H|T]) ->
  case findMember(Name,Arity,H) of
    undefined -> findMember(Name,Arity,T);
    Res -> Res
  end;
findMember(Name,Arity,Member=#member{name=Name,arity=Arity}) -> Member;
findMember(_Name,_Arity,_) -> undefined.

fieldMembers(_Module,Fields) ->
  [ [ case F#field.getter of
        undefined -> [];
        _ ->
          erl_syntax:attribute(?atom(member), [ ?abstract(
            {{F#field.getter,0},if F#field.getter_public -> [public]; true -> [private] end ++ [accessor,{mangle,false}]}
          )])
      end
    % , erl_syntax:attribute(?atom(member), [ ?tuple([
    %     erl_syntax:arity_qualifier(?atom(F#field.setter),?integer(1)),
    %     ?abstract(if F#field.setter_public -> [public]; true -> [private] end ++ [mutator,{mangle,false}])
    %   ])])
    , case F#field.setter of
        undefined -> [];
        _ ->
          erl_syntax:attribute(?atom(member), [ ?abstract(
            {{F#field.setter,1},if F#field.setter_public -> [public]; true -> [private] end ++ [mutator,{mangle,false}]}
          )])
      end
    ] || F <- Fields
  ] ++ [erl_syntax:attribute(?atom(member), [ ?abstract( {{update,1},[public,mutator,{mangle,false}]} ) ])].
fieldLogic(Module,Fields) ->
  ?debug("fieldLogic(~p,~p)",[Module,Fields]),
  [ [ case F#field.getter of 
        undefined -> [];
        _ ->
          [ ?suppress_unused(F#field.getter,1)
          , fieldGetter(F,Module,Fields)
          ]
      end
    , case F#field.setter of
        undefined -> [];
        _ ->
          [ ?suppress_unused(F#field.setter,2)
          , fieldSetter(F,Module,Fields)
          ]
      end
    ] || F<-Fields ]
  ++ [ updateLogic(Module,Fields) ].
fieldGetter(#field{name=Name,getter=Getter,default=Default},Module,_Fields) ->
  ?function(Getter,
  case Default of
    no_default -> [ ?clause([?record(Module,[?field(Name,?atom('#UNDEF#'))])],none,[?apply(erlang,error,[?atom(no_value)])]) ];
    _ -> []
  end ++
  [ ?clause([?record(Module,[?field(Name,?var('Value'))])],none,[?var('Value')])
  ]).
fieldSetter(#field{name=Name,setter=Setter},Module,_Fields) ->
  ?function(Setter,
  [ ?clause([?var('Value'), ?match(?var('Self'),?record(Module,[]))], none,
      [ ?record(?var('Self'),Module,[?field(Name,?var('Value'))]) ])
  ]).

initLogic(Module,Fields) ->
  [ ?suppress_unused(init,length([ F || F=#field{default=no_default} <- Fields ]))
  , ?function(init, [?clause([?var('FIELD',F#field.name) || F=#field{default=no_default} <- Fields ],none,
      [?record(Module,[ ?field(F#field.name, case F#field.default of no_default -> ?var('FIELD',F#field.name);
                                                                                                       #default{value=Value} -> ?abstract(Value) end)
                                                          || F<-Fields ])])])
  ].
memberExports(_Module,Members,_Fields) ->
  % [ erl_syntax:attribute(?atom(export), [?abstract([{Name,Arity+1}])])
  [ erl_syntax:attribute(?atom(export), [?list([erl_syntax:arity_qualifier(?atom(Name),?int(Arity+1))])])
    || #member{name=Name,arity=Arity,public=true} <- Members ].


updateLogic(Module,Fields) ->
  VarSource = [ ?var("_Source_"++integer_to_list(I)) || I <- lists:seq(1,length(Fields)) ],
  VarDest = [ ?var("_Dest_"++integer_to_list(I)) || I <- lists:seq(1,length(Fields)) ],
  [ ?function(update, [ ?clause([ ?record(Module,[ ?field(F#field.name,V) || {F,V} <- lists:zip(Fields,VarSource) ])
                                , ?record(Module,[ ?field(F#field.name,V) || {F,V} <- lists:zip(Fields,VarDest) ])
                                ], none, [?record(Module, [ ?field(F#field.name,updateLogic(F#field.update,S,D)) || {F,{S,D}} <- lists:zip(Fields,lists:zip(VarSource,VarDest)) ])])]) ].

updateLogic(immutable,_Source,Dest) -> Dest;
updateLogic(replace,Source,_Dest) -> Source;
updateLogic(update,Source,Dest) -> erl_syntax:application(Dest,?atom(update),[Source]);
updateLogic(#ifdef{value=Undef,update=Update},Source,Dest) ->
  ?cases(Source,[?clause([?abstract(Undef)],none,[Dest]),?clause([?underscore],none,[updateLogic(Update,Source,Dest)])]);
updateLogic({Mod,Fun},Source,Dest) when is_atom(Mod), is_atom(Fun) -> ?apply(Mod,Fun,[Source,Dest]);
updateLogic(Fun,Source,Dest) when is_atom(Fun) -> ?apply(Fun,[Source,Dest]).
                               




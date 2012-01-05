-module(value_pt).

-export([parse_transform/2]).

-include("debug.hrl").

-include("parsetransform.hrl").

parse_transform(Forms0,_Options) ->
  Forms1 = ?forms(Forms0),
  {_,Module} = 
    syntax_fold(fun get_module/2, Forms1,'_'),
  {_,Fields} = syntax_fold(fun get_fields/2,Forms1,[]),
  {_,Members0} = syntax_fold(fun get_members/2,Forms1,[]),
  case {Fields,Members0} of
    {[],[]} -> Forms0;
    _ -> 
      Forms2 = insert_in_head(Forms1, field_members(Module,Fields)),
      {_,Members} = syntax_fold(fun get_members/2,Forms2,[]),
      Forms3 = insert_at_top(Forms2, module_record(Module,Fields)),
      Forms4 = insert_at_end(Forms3, init_logic(Module,Fields)),
      Forms5 = insert_at_end(Forms4, field_logic(Module,Fields)),
      Forms6 = insert_in_head(Forms5, member_exports(Module,Members,Fields)),
      Forms7 = syntax_fold(fun (E) -> mangle_members(E,Module,Members,Fields) end,Forms6),
      ?debug_pt(Forms7),
      erl_syntax:revert_forms(Forms7)
  end.

get_module(Form,'_') ->
  case erl_syntax:type(Form) of
    attribute ->
      case erl_syntax_lib:analyze_attribute(Form) of
        {module,Module} -> {done,Module};
        _ -> done
      end;
    _ -> skip
  end;
get_module(_,_) -> skip.
get_fields(Form,Fields) ->
  case erl_syntax:type(Form) of
    attribute ->
      case analyze_attribute(Form) of
        {field,Field} -> {done,Fields ++ [ field(Field) ]};
        _ -> done
      end;
    _ -> skip
  end.

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

get_members(Form,Members) ->
  case erl_syntax:type(Form) of
    attribute ->
      case analyze_attribute(Form) of
        {member,Member} -> {done, Members ++ [ member(Member) ] };
        _ -> done
      end;
    _ -> skip
  end.

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

mangle_members(Form,Module,Members,Fields) ->
  case erl_syntax:type(Form) of
    function ->
      Function = erl_syntax:function_name(Form),
      Name =  erl_syntax:atom_value(Function),
      Arity = erl_syntax:function_arity(Form),
      Clauses = erl_syntax:function_clauses(Form),
      case find_member(Name,Arity,Members) of
        #member{mangle=true,mutator=Mutator} ->
          {done, erl_syntax:function(Function, [ mangle_member_clause(Name,Arity,Mutator,C,Module,Members,Fields) || C <- Clauses ]) };
        _ -> skip
      end;
    _ -> skip
  end.

new_this(Vars0) -> 
  This1 = erl_syntax_lib:new_variable_name(fun (N) -> list_to_atom("_THIS_"++integer_to_list(N)) end,Vars0),
  Vars1 = sets:add_element(This1,Vars0),
  {?var(This1),Vars1}.

mangle_member_clause(_Name,_Arity,Mutator,Clause,_Module,Members,_Fields) -> 
  Pattern = erl_syntax:clause_patterns(Clause) ++ [erl_syntax:variable('THIS')],
  Guard = erl_syntax:clause_guard(Clause),
  Body = erl_syntax:clause_body(Clause) ++ case Mutator of true -> [erl_syntax:variable('THIS')]; _ -> [] end,
  {NewClause,_} = erl_syntax_lib:mapfold (
      fun(Expr,{This,Vars}) ->
        case erl_syntax:type(Expr) of
          variable -> case erl_syntax:variable_name(Expr) of 'THIS' -> {This,{This,Vars}}; _ -> {Expr,{This,Vars}} end;
          application ->
            Operator = erl_syntax:application_operator(Expr),
            Arguments = erl_syntax:application_arguments(Expr),
            case erl_syntax:type(Operator) of
              atom ->
                FunName = erl_syntax:atom_value(Operator),
                FunArity = length(Arguments),
                case find_member(FunName,FunArity,Members) of
                  Fun = #member{} ->
                    NApply = erl_syntax:application(Operator,Arguments++[This]),
                    case Mutator andalso Fun#member.mutator of
                      true ->
                        {NThis,NVars} = new_this(Vars),
                        {erl_syntax:match_expr(NThis,NApply),{NThis,NVars}};
                      _ -> {NApply,{This,Vars}}
                    end;
                  _ -> {Expr,{This,Vars}}
                end;
              _ -> {Expr,{This,Vars}}
            end;
          _ -> {Expr,{This,Vars}}
        end
      end , new_this(erl_syntax_lib:variables(Clause))
          , erl_syntax:clause(Pattern,Guard,Body)),
  NewClause.
  

find_member(_Name,_Arity,[]) -> undefined;
find_member(Name,Arity,[H|T]) ->
  case find_member(Name,Arity,H) of
    undefined -> find_member(Name,Arity,T);
    Res -> Res
  end;
find_member(Name,Arity,Member=#member{name=Name,arity=Arity}) -> Member;
find_member(_Name,_Arity,_) -> undefined.

field_members(_Module,Fields) ->
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
field_logic(Module,Fields) ->
  [ [ case F#field.getter of 
        undefined -> [];
        _ ->
          [ ?suppress_unused(F#field.getter,1)
          , field_getter(F,Module,Fields)
          ]
      end
    , case F#field.setter of
        undefined -> [];
        _ ->
          [ ?suppress_unused(F#field.setter,2)
          , field_setter(F,Module,Fields)
          ]
      end
    ] || F<-Fields ]
  ++ [ update_logic(Module,Fields) ].
field_getter(#field{name=Name,getter=Getter,default=Default},Module,_Fields) ->
  ?function(Getter,
  case Default of
    no_default -> [ ?clause([?record(Module,[?field(Name,?atom('#UNDEF#'))])],none,[?apply(erlang,error,[?atom(no_value)])]) ];
    _ -> []
  end ++
  [ ?clause([?record(Module,[?field(Name,?var('Value'))])],none,[?var('Value')])
  ]).
field_setter(#field{name=Name,setter=Setter},Module,_Fields) ->
  ?function(Setter,
  [ ?clause([?var('Value'), ?match(?var('This'),?record(Module,[]))], none,
      [ ?record(?var('This'),Module,[?field(Name,?var('Value'))]) ])
  ]).

init_logic(Module,Fields) ->
  [ ?suppress_unused(init,length([ F || F=#field{default=no_default} <- Fields ]))
  , ?function(init, [?clause([?var('FIELD',F#field.name) || F=#field{default=no_default} <- Fields ],none,
      [?record(Module,[ ?field(F#field.name, case F#field.default of no_default -> ?var('FIELD',F#field.name);
                                                                                                       #default{value=Value} -> ?abstract(Value) end)
                                                          || F<-Fields ])])])
  ].
member_exports(_Module,Members,_Fields) ->
  % [ erl_syntax:attribute(?atom(export), [?abstract([{Name,Arity+1}])])
  [ erl_syntax:attribute(?atom(export), [?list([erl_syntax:arity_qualifier(?atom(Name),?int(Arity+1))])])
    || #member{name=Name,arity=Arity,public=true} <- Members ].


update_logic(Module,Fields) ->
  VarSource = [ ?var("_Source_"++integer_to_list(I)) || I <- lists:seq(1,length(Fields)) ],
  VarDest = [ ?var("_Dest_"++integer_to_list(I)) || I <- lists:seq(1,length(Fields)) ],
  [ ?function(update, [ ?clause([ ?record(Module,[ ?field(F#field.name,V) || {F,V} <- lists:zip(Fields,VarSource) ])
                                , ?record(Module,[ ?field(F#field.name,V) || {F,V} <- lists:zip(Fields,VarDest) ])
                                ], none, [?record(Module, [ ?field(F#field.name,update_logic(F#field.update,S,D)) || {F,{S,D}} <- lists:zip(Fields,lists:zip(VarSource,VarDest)) ])])]) ].

update_logic(immutable,_Source,Dest) -> Dest;
update_logic(replace,Source,_Dest) -> Source;
update_logic(update,Source,Dest) -> erl_syntax:application(Dest,?atom(update),[Source]);
update_logic(#ifdef{value=Undef,update=Update},Source,Dest) ->
  ?cases(Source,[?clause([?abstract(Undef)],none,[Dest]),?clause([?underscore],none,[update_logic(Update,Source,Dest)])]);
update_logic({Mod,Fun},Source,Dest) when is_atom(Mod), is_atom(Fun) -> ?apply(Mod,Fun,[Source,Dest]);
update_logic(Fun,Source,Dest) when is_atom(Fun) -> ?apply(Fun,[Source,Dest]).
                               




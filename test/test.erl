-module(test).

-compile([{parse_transform,value_pt}]).

-field({name,[{default,undefined},public]}).
-field({attrs,[{default,[]},public]}).
-field({body,[{default,[]},public]}).

-member({fun1/0,[accessor,public]}).
-member({fun2/0,[accessor,public]}).
-member({fun3/0,[accessor,public]}).
-member({fun4/0,[mutator,public]}).
-member({fun5/0,[mutator,public]}).
-member({fun6/0,[mutator,public]}).

-export([init/0]).


fun1() ->
  try
    name(name())
  of
    x -> attrs(attrs());
    y -> ok
  catch
    A:B -> ok;
    E -> not_ok
  after
    name(x)
  end.

fun2() ->
  case
    name(name())
  of
    x -> attrs(attrs());
    y -> ok
  end.

fun3() ->
  receive
    A -> name(A), body(b)
  after 100 -> name(time_out)
  end.

fun4() ->
  try
    name(name())
  of
    x -> attrs(attrs());
    y -> ok
  catch
    A:B -> ok;
    E -> not_ok
  after
    name(x)
  end.

fun5() ->
  case
    name(name())
  of
    x -> attrs(attrs());
    y -> ok
  end.

fun6() ->
  receive
    A -> name(A), body(b)
  after 100 -> name(time_out)
  end.






-module(test).

-compile([{parse_transform,value_pt}]).

-field({name,[{default,undefined},public]}).
-field({attrs,[{default,[]},public]}).
-field({body,[{default,[]},public]}).

-export([init/0]).



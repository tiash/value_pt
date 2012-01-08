Value Parsetransform
====================

This parse transform makes it easy to define value types.

The generated code is Parametric module compatible, but more flexible.

The generated code is likely not suitable for persistance, I would advise using
some form of serialisation.

Usage
-----
1. add `-compile([{parse_transform,value_pt}])` to your code.
2. add `-field({Name,Opts})` declarations for all the fields you need
3. add `-member({Name/Arity,Opts})` declarations for all member functions.

Fields
------
Fields are declared using `-field({Name,Opts})` or simply `-field(Name)`.

For each delcaration a getter and setter and the appropriate member declarations
are generated:

    -member({Name/0,[no_mangle,accessor|Opts]).
    Name(#?MODULE{Name=Value}) -> Value.
    -member({Name/1,[no_mangle,mutator|Opts]).
    Name(Value,This) -> This#?MODULE{Name=Value}.

The transformer will also generate the an appropriate record declaration and
init function.

The record is simply all the fields in the declared order.

The init function takes all fields with no default in that order as arguments.

###Options

* `{getter,Fun}` the setter function (`Fun/1`), defaults to `Name`.

* `{setter,Fun}` the getter function (`Fun/2`), defaults to `Name`.

* `public`, `getter_public`, `setter_public`, `private`, `getter_private`,
  `setter_private` wether or not the getter/setter should be exported, defaults
  to `private`.
  
* `{default,Default}`, `no_default` default value to initialise the field with,
  defaults to `no_default`.


Members
-------
Fields are declared using `-member({Name/Arity,Opts})` or simply
`-member(Name/Arity)`.

Each of these functions is rewritten to take an extra last argument
(`This :: #?MODULE`) and it is passed to all called members.

###Options

* `public`, `private` wether or not the function should be exported, defaults
  to `private`.

* `mutator`, `accessor` indicates that the function is a mutator, defaults to
  `accessor`. If set to `mutator` occurances of THIS will be updated when
  calling other mutators and the function will return the updated `THIS`.

* `no_mangle` disables code rewriting, the function must already take an extra
  argument `THIS` and handle it appropriately. Usefull to integrate "hand
  written" code with the mangled code (eg for getters and setters).


Notes
-----
* All language constructs should be handled appropriately;

* function expressions are ignored

* There is an issue with `try ... of ...` expressions;
  If the body contains a mutator then the branches will complain about unsafe
  variables, no such issue arrises with `case ... of ...`.

* The meaning of `THIS` is a bit weird in
  `try ... of ... catch .. after ... end`.
  
    * the `of` clauses are evaluated AFTER the `try` body, so the sequence of
      `THIS` is ovious here;

    * catch clauses start with the `THIS` from before the `try` block, however
      terminate on the same `THIS` as the body+clauses;
  
    * the after clause starts witht the `THIS` from before the `try` and its
      final `THIS` is discarded.

    * In theory it should be possible to Safely access `THIS` after a try, if
      the body was only partially evaluated due to an error, non of the changes
      to `THIS` will be reflected; generally this is ok, however it is important
      to be aware of this.
  
  



     


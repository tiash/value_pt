Value Parsetransform
====================

This parse transform makes it easy to define value types.

The generated code is Parametric module compatible, but more flexible.

The generated code is likely not suitable for persistance, I would advise using some form of serialisation.

Usage
-----
1. add `-compile([{parse_transform,value_pt}])` to your code.
2. add `-field({Name,Opts})` declarations for all the fields you need

  a pair of getter `Name(Self)->Value` and setter `Name(Value,Self)->Self` functions is generated.

  a pair of `-member({Name/0,[no_mangle|Opts]})` and `-member({Name/1,[mutator,no_mangle,Opts]})` is generated for the getter and setter.

  Valid options are:

  * `{getter,Fun}` the setter function (`Fun/1`), defaults to `Name`.

  * `{setter,Fun}` the getter function (`Fun/2`), defaults to `Name`.

  * `public`, `getter_public`, `setter_public`, `private`, `getter_private`, `setter_private` wether or not the getter/setter should be exported, defaults to `private`.
  
  * `{default,Default}`, `no_default` default value to initialise the field with, defaults to `no_default`.

3. add `-member({Name/Arity,Opts})` declarations for all member functions.
  
  The functions will be modified to take an extra last argument `THIS :: #?MODULE{}`, and this argument is passed to all invocations of other -member(...) functions.
  
  Valid options are:

  * `public`, `private` wether or not the function should be exported, defaults to `private`.

  * `mutator`, `accessor` indicates that the function is a mutator, defaults to `accessor`.
  If set to `mutator` occurances of THIS will be updated when calling other mutators and the function will return the updated `THIS`.

  * `accessor` indicates that the function is not a mutator, will not modify occurances of `THIS` and will give the result the user specificied (default.

  * `no_mangle` disables code rewriting, the function must already take an extra argument `THIS` and handle it appropriately. Usefull to integrate "hand written" code with the mangled code (eg for getters and setters).


The transformer will also generate an appropriate record declaration: `-record(?MODULE,{Field1...})` (with the fields in the declared order) and an init function: `init(Val...) -> #?MODULE{}` with arguments for all fields without a default.


     


module Lang = Lang

let sexp_of_program = Lang.sexp_of_program

let run (prog : Parse.program) : Lang.program =
  prog |> Simplify.run |> Typeit.run |> Update.run |> Wrap.run

(* NOTE, Upates for references should be part of the struct. I.e., the value *)
(* is the `box_value` slot, but the updater is a `broadcast` slot. Then, *)
(* when a place is written its `broadcast` function is called. This way, we *)
(* don't have to change signatures or do anything else besides just working with variables. *)

(* NOTE, do free variables before lfting. All local bindings are lifted *)
(* to `this`, but arguments aren't. so free variables passed to the function *)
(* won't be. We only need to lift `LabelE` defined functions. *)

(* TODO: *)
(* 1. Capture free variables and lift them up. We need to do this *)
(* because all functions will be created with a unique object backing. *)
(* E.G., () => { let f = function f() {}; return f.bind({}) } *)
(* Free variables and their updating functions (for written refs) *)
(* should be passed to the initialization function. *)

(* 2. All local bindings should be placed on `this`. We do this so that *)
(* the updaters can be created and reference variables declared lexically later. *)

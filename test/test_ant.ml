[@@@warning "-26"]

open Ant.Ty

let pp_error ppf (`Msg x) = Format.pp_print_string ppf x

let error = Alcotest.testable pp_error ( = )

let types_that_should_unify =
  let x = Symbol.var "x" and y = Symbol.var "y" in
  [ (Ty.number, Ty.number)
  ; (Ty.string, Ty.string)
  ; (Ty.bool, Ty.bool)
  ; (Ty.ref Ty.number, Ty.ref (Ty.new_meta ()))
  ; ( Ty.arrow [(x, Ty.number)] Ty.string
    , Ty.arrow [(x, Ty.new_meta ())] (Ty.new_meta ()))
  ; ( Ty.arrow [(x, Ty.number)] Ty.number ~deps:(Dependencies.of_list [(Dep.on Place.result [Place.baseid x])] |> Ty.dependencies)
    , Ty.arrow [(x, Ty.number)] Ty.number ~deps:(Dependencies.of_list [(Dep.on Place.result [Place.baseid x])] |> Ty.dependencies))
  ][@@ocamlformat "disable"]

let types_unify () =
  List.iter
    (fun (t1, t2) ->
      let actual = Ty.unify t1 t2 in
      let expected = Ok () in
      Alcotest.(check (result unit error) "unifies" expected actual) )
    types_that_should_unify

let unification_suite = [("types_unify", `Quick, types_unify)]

let () = Alcotest.run "Unification" [("unification", unification_suite)]

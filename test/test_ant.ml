[@@@warning "-26"]

open Ant.Ty
open Ant.Util

let pp_error ppf (`Msg x) = Format.pp_print_string ppf x

let error = Alcotest.testable pp_error ( = )

let x = Symbol.var "x"

let types_that_should_unify =
  [ (Ty.number, Ty.number)
  ; (Ty.string, Ty.string)
  ; (Ty.bool, Ty.bool)
  ; (Ty.ref Ty.number, Ty.ref (Ty.new_meta ()))
  ; ( Ty.arrow [(x, Ty.number)] Ty.string
    , Ty.arrow [(x, Ty.new_meta ())] (Ty.new_meta ()))
  ; ( Ty.arrow [(x, Ty.number)] Ty.number ~deps:(Dependencies.of_list [(Dep.on Place.result [Place.baseid x])] |> Ty.dependencies)
    , Ty.arrow [(x, Ty.number)] Ty.number ~deps:(Dependencies.of_list [(Dep.on Place.result [Place.baseid x])] |> Ty.dependencies))
  ][@@ocamlformat "disable"]

let occurs_expands_metas () =
  let m = Ty.new_meta () in
  let res_unit = Ty.unify m (Ty.ref Ty.number) in
  Alcotest.(check (result unit error) "unifies" (Ok ()) res_unit) ;
  Alcotest.(check bool) "does not occur in" false (Ty.occurs m m)

let generalization_expands_metas () =
  let deps = Ty.dependencies Dependencies.empty
  and effs = Ty.effects Effects.empty in
  let f1 = Ty.func ~deps ~effs [(x, Ty.new_meta ())] (Ty.new_meta ()) in
  let f2 = Ty.func ~deps ~effs [(x, Ty.number)] Ty.number in
  let actual = Ty.unify f1 f2 in
  let expected = Ok () in
  Alcotest.(check (result unit error) "unifies" expected actual) ;
  let g = Ty.generalize SymbolMap.empty f1 in
  let s = Signature.from_ty g |> Result.unwrap in
  Alcotest.(check int) "no tyvars" 0 (List.length s.tyvars)

let types_unify () =
  List.iter
    (fun (t1, t2) ->
      let actual = Ty.unify t1 t2 in
      let expected = Ok () in
      Alcotest.(check (result unit error) "unifies" expected actual) )
    types_that_should_unify

let unification_suite =
  [ ("types_unify", `Quick, types_unify)
  ; ("generalization_expands_metas", `Quick, generalization_expands_metas)
  ; ("occurs_expands_metas", `Quick, occurs_expands_metas) ]

let () = Alcotest.run "Unification" [("unification", unification_suite)]

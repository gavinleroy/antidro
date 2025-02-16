[@@@warning "-26"]

open Ant.Ty
open Ant.Util

let error = Alcotest.testable Error.pp ( = )

let x = Symbol.var "x"

let no_effs = Effects.to_ty Effects.empty

let no_deps = Dependencies.to_ty Dependencies.empty

let types_that_should_unify =
  let x = Symbol.var "x" and y = Symbol.var "y" in
  [ Ty.number, Ty.number
  ; Ty.string, Ty.string
  ; Ty.bool, Ty.bool
  ; Ty.ref Ty.number, Ty.ref (Ty.new_meta ())
  ; (let t = TyVar.fresh () in VarT t, VarT t)
  ; Ty.new_meta (), Ty.bool
  ; Ty.string, Ty.new_meta ()
  ; Ty.arrow [(x, Ty.number)] Ty.string
    , Ty.arrow [(x, Ty.new_meta ())] (Ty.new_meta ())
  ; Ty.arrow [(x, Ty.number)] Ty.number ~deps:(Dependencies.of_list [(Dep.on Place.return [Place.baseid x])] |> Dependencies.to_ty)
    , Ty.arrow [(x, Ty.number)] Ty.number ~deps:(Dependencies.of_list [(Dep.on Place.return [Place.baseid x])] |> Dependencies.to_ty)
  ; ( let t = TyVar.fresh ()  in
      Ty.arrow
        ~tyvars:[t]
        ~effs:no_effs
        ~deps:no_deps
        [x, Ty.array (VarT t)]
        Ty.bool
    , Ty.arrow
        ~effs:no_effs
        ~deps:no_deps
        [x, Ty.array Ty.string]
        Ty.bool )
  ; ( let t = TyVar.fresh ()  in
      Ty.func
        ~effs:no_effs
        ~deps:no_deps
        [x, Ty.array (Ty.new_meta ())]
        Ty.bool
      |> Ty.generalize Ty.SymbMap.empty
    , Ty.arrow
        ~effs:no_effs
        ~deps:no_deps
        [x, Ty.array Ty.string]
        Ty.bool )
  ; ( let t = TyVar.fresh ()  in
      Ty.arrow
        ~tyvars:[t]
        ~effs:no_effs
        ~deps:no_deps
        [x, Ty.array (VarT t)]
        Ty.bool
    , Ty.arrow
        ~effs:no_effs
        ~deps:no_deps
        [y, Ty.array Ty.string]
        Ty.bool )
  ; ( let t = TyVar.fresh () and u = TyVar.fresh () in
      Ty.arrow
        ~tyvars:[t]
        ~effs:no_effs
        ~deps:no_deps
        [x, Ty.array (VarT t); y, Ty.bool]
        Ty.bool
    , Ty.arrow
        ~tyvars:[u]
        ~effs:no_effs
        ~deps:no_deps
        [y, Ty.array Ty.string; x, (VarT u)]
        Ty.bool )
  ; ( let t = TyVar.fresh () and u = TyVar.fresh () and r = TyVar.fresh () and e = TyVar.fresh () in
      (* forall<t, u, r, e> (x: t) -> u | r # e *)
      Ty.arrow
        ~tyvars:[t; u; r; e]
        ~effs:(VarT e)
        ~deps:(VarT r)
        [x, VarT t]
        (VarT u)
      (* (y: Bool[]) -> Bool | result^{y[0]} # {} *)
    , Ty.arrow
        ~effs:no_effs
        ~deps:(Dependencies.to_ty (Dependencies.of_list [Dep.on Place.return [Place.offset 0 (Place.baseid y)]]))
        [y, Ty.array Ty.bool]
        Ty.bool )
  ][@@ocamlformat "disable"]

let types_that_dont_unify =
  let x = Symbol.var "x" and y = Symbol.var "y" in
  [ Ty.number, Ty.string
  ; Ty.number, Ty.bool
  ; Ty.string, Ty.bool
  ; Ty.ref Ty.number, Ty.ref Ty.string
  ; (let t = TyVar.fresh () in
     VarT t, (Ty.func ~deps:no_deps ~effs:no_effs [(x, VarT t)] (VarT t)))
  ; (let t = TyVar.fresh () in
     VarT t, Ty.ref (VarT t))
  ][@@ocamlformat "disable"]

let occurs_expands_metas () =
  let m = Ty.new_meta () in
  let res_unit = Ty.unify m (Ty.ref Ty.number) in
  Alcotest.(check (result unit error) "unifies" (Ok ()) res_unit) ;
  Alcotest.(check bool) "does not occur in" false (Ty.occurs m m)

let generalization_expands_metas () =
  let deps = Dependencies.to_ty Dependencies.empty
  and effs = Effects.to_ty Effects.empty in
  let f1 = Ty.func ~deps ~effs [(x, Ty.new_meta ())] (Ty.new_meta ()) in
  let f2 = Ty.func ~deps ~effs [(x, Ty.number)] Ty.number in
  let actual = Ty.unify f1 f2 in
  let expected = Ok () in
  Alcotest.(check (result unit error) "unifies" expected actual) ;
  let g = Ty.generalize Ty.SymbMap.empty f1 in
  let s = Signature.from_ty g |> Result.unwrap in
  Alcotest.(check int) "no tyvars" 0 (List.length s.tyvars)

let inst_and_unify a b = Ty.unify (Ty.instantiate a) (Ty.instantiate b)

let types_unify () =
  List.iteri
    (fun i (t1, t2) ->
      let actual = inst_and_unify t1 t2 in
      let expected = Ok () in
      let name = Printf.sprintf "unifies: %d" i in
      Alcotest.(check (result unit error) name expected actual) )
    types_that_should_unify

let types_dont_unify () =
  List.iteri
    (fun i (t1, t2) ->
      let actual = inst_and_unify t1 t2 |> Result.is_error in
      let expected = true in
      let name = Printf.sprintf "does not unify: %d" i in
      Alcotest.(check bool name expected actual) )
    types_that_dont_unify

let unification_suite =
  [ ("types_unify", `Quick, types_unify)
  ; ("types_dont_unify", `Quick, types_dont_unify)
  ; ("generalization_expands_metas", `Quick, generalization_expands_metas)
  ; ("occurs_expands_metas", `Quick, occurs_expands_metas) ]

let () = Alcotest.run "Unification" [("unification", unification_suite)]

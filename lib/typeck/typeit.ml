[@@@warning "-a"]

open Env
open Lang
open Ant.Ty
open Ant.Util
open ResultMonad

let rec typeck_place (sv : Gamma.t) (st : Theta.t) (pl : Place.t) :
    (Ty.t, 'e) result =
  failwith "NYI"

and typeck_expr (sv : Gamma.t) (st : Theta.t) : expr -> (Ty.t, 'e) result =
  function
  | VoidE ->
      return Ty.void
  | TrueE ->
      return Ty.bool
  | FalseE ->
      return Ty.bool
  | LitE _ ->
      return Ty.number
  | StringE _ ->
      return Ty.string
  | PlaceE pl ->
      typeck_place sv st pl
  | IfE (test, consequence, alternative) ->
      let* tty = typeck_expr sv st test in
      let* cty = typeck_expr sv st consequence in
      let* aty = typeck_expr sv st alternative in
      let* () = Ty.unify Ty.bool tty in
      let+ () = Ty.unify cty aty in
      cty
  | LetE (id, ty, e, body) ->
      let* ety = typeck_expr sv st e in
      let* () = Ty.unify ty ety in
      typeck_expr sv (* (Gamma.insert id ty sv) *) st body
  | RefE e ->
      let+ ty = typeck_expr sv st e in
      Ty.ref ty
  | StructE fields ->
      let+ _ =
        List.all
          (fun (slot, ty, expr) ->
            let* t = typeck_expr sv st expr in
            Ty.unify ty t )
          fields
      in
      Ty.struct_ (List.map (fun (s, t, _) -> (s, t)) fields)
  | SetE (pl, expr) ->
      let* plty = typeck_place sv st pl in
      let* ety = typeck_expr sv st expr in
      let+ () = Ty.unify plty (Ty.ref ety) in
      Ty.void
  | FnE (ty, body) ->
      failwith "NYI"
  | AppE {fty; tyargs; f; args} ->
      failwith "NYI"

let report_type_error (`Msg str) =
  Format.eprintf "Type error: %s@." str ;
  exit 1

let run (prog : program) : program =
  let run_inner () =
    let* ty = typeck_expr Gamma.initial Theta.initial prog in
    Ty.unify Ty.void ty
  in
  run_inner () |> function Ok () -> prog | Error err -> report_type_error err

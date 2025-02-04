open Ant.Ty
open Ant.Util
open Lang

let run (p : program) : program =
  let main = Symbol.var "antidro_toplevel" in
  let main_ty =
    Ty.arrow ~deps:(DepsT Dependencies.empty) ~effs:(EffsT Effects.empty) []
      Ty.void
  in
  LetE
    { bound= main
    ; ty= main_ty
    ; deps= Dependencies.empty
    ; expr=
        FnE
          ( Signature.from_ty main_ty |> Result.unwrap |> Signature.formals
          , List.fold_right
              (fun (`Method (_, _, overloads)) (body : program) ->
                List.fold_right
                  (fun (id, ty, js) (body : program) ->
                    let sg = Signature.from_ty ty |> Result.unwrap in
                    LetE
                      { bound= id
                      ; ty
                      ; expr= FnE (Signature.formals sg, InlineJSE js)
                      ; deps= Dependencies.empty
                      ; body } )
                  overloads body )
              AntStd.pervasives p )
    ; body= AppE {f= Place.baseid main; args= []} }

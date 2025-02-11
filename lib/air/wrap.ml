open Ant.Ty
open Ant.Util
open Lang

let make_let id ty impl =
  let sg = Signature.from_ty ty |> Result.unwrap in
  LetS (id, FnE (Signature.formals sg, [JSBody impl]))

let run (p : program) : program =
  List.concat_map
    (function
      | AntStd.MethodPrim {overloads; _} ->
          List.map
            (fun ({id; ty; impl} : AntStd.overload_data) -> make_let id ty impl)
            overloads
      | AntStd.FuncPrim {id; ty; impl; _} ->
          [make_let id ty impl] )
    AntStd.pervasives
  |> fun defs -> List.append defs p

open Lang

let rec convert_expr expr =
  match expr with
  | Typeck.VoidE ->
      VoidE
  | Typeck.TrueE ->
      TrueE
  | Typeck.FalseE ->
      FalseE
  | Typeck.LitE i ->
      LitE i
  | Typeck.StringE s ->
      StringE s
  | Typeck.NewE (id, freevars) ->
      NewE (id, freevars)
  | Typeck.PlaceE pl ->
      PlaceE pl
  | Typeck.StructE bindings ->
      StructE bindings
  | Typeck.FnE (psig, body) ->
      FnE (psig, convert_expr body)
  | Typeck.AppE (aid, args) ->
      AppE (aid, args)
  | Typeck.SetE (pl, id) ->
      SetE (pl, id)
  | Typeck.LetE (bound, ty, e, body) ->
      LetE (bound, ty, convert_expr e, convert_expr body)

let run (p : Typeck.program) : program = {expr= convert_expr p.expr; ty= p.ty}

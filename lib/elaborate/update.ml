open Lang

let add_expression_updater bound ty toexpr body =
  let setter_id = Place.baseid bound |> Place.to_updater_id in
  let tmp_id = Identifier.fresh () in
  LetE
    ( bound
    , ty
    , toexpr
    , LetE
        ( setter_id
        , FnT (Signature.basic [] VoidT)
        , LamE ([], LetE (tmp_id, ty, toexpr, SetE (Place.baseid bound, tmp_id)))
        , body ) )

let rec update_expr expr =
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
  | Typeck.PlaceE pl ->
      PlaceE pl
  | Typeck.StructE bindings ->
      StructE bindings
  | Typeck.FnE (psig, body) ->
      FnE (psig, upd_expr body)
  | Typeck.AppE (aid, args) ->
      AppE (aid, args)
  | Typeck.SetE (pl, id) ->
      SetE (pl, id)
  | Typeck.LetE (bound, StructT tybindings, StructE bindings, body) ->
      let body = upd_expr body in
      List.fold_right
        (fun (slot, id) body ->
          let setter_id = Place.to_updater_id (Place.slotid slot bound) in
          LetE
            ( setter_id
            , FnT (Signature.basic [] VoidT)
            , LamE ([], SetE (Place.slotid slot bound, id))
            , body ) )
        bindings body
      |> add_expression_updater bound (StructT tybindings) (StructE bindings)
  | Typeck.LetE (bound, ty, toexpr, body) ->
      let toexpr = upd_expr toexpr and body = upd_expr body in
      add_expression_updater bound ty toexpr body

let run (p : program) : program = {expr= upd_expr p.expr; ty= p.ty; rhos= p.rhos}

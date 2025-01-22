open Lang

let rec wrap_expr expr (ctx : Rho.t IdMap.t) (rho : Rho.t) =
  let rho_of id = IdMap.find_opt id ctx |> Option.value ~default:rho in
  match expr with
  | LetE (bound, FnT psig, PlaceE (Place.VarB fid, []), body)
    when Signature.is_effectful psig ->
      let body = wrap_expr body ctx rho
      and tmpf = Identifier.fresh ()
      and tmpret = Identifier.fresh ()
      and args = Signature.args psig in
      let ectx hole =
        LetE
          ( bound
          , FnT psig
          , LetE
              ( tmpf
              , FnT psig
              , PlaceE (Place.baseid fid)
              , LamE
                  ( args
                  , LetE (tmpret, Signature.return psig, AppE (tmpf, args), hole)
                  ) )
          , body )
      in
      List.fold_right
        (fun (Typeck.WriteEf pl) body ->
          List.fold_right
            (fun pl body ->
              LetE
                ( Identifier.fresh ()
                , VoidT
                , AppE (Place.to_updater_id pl, [])
                , body ) )
            (Rho.downstream_of pl rho) body )
        (Signature.effects psig)
        (PlaceE (Place.baseid tmpret))
      |> ectx
  | LetE (bound, ty, boundto, body) ->
      LetE
        (bound, ty, wrap_expr boundto ctx (rho_of bound), wrap_expr body ctx rho)
  | expr ->
      expr

let run (p : program) : program =
  {p with expr= wrap_expr p.expr p.rhos Rho.initial}

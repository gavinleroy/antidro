module P = Parse
include Environments
include Lang

let remember_rho, global_ctx =
  let ctx : Rho.t IdMap.t ref = ref IdMap.empty in
  ((fun id rho -> ctx := IdMap.add id rho !ctx), fun () -> !ctx)

let expect expected actual err cont =
  if expected = actual then cont () else err ()

let mapk f ls k =
  let rec loop acc = function
    | [] ->
        k (List.rev acc)
    | x :: xs ->
        f x (fun x' -> loop (x' :: acc) xs)
  in
  loop [] ls

let check_for_duplicates symbols err cont =
  let rec loop = function
    | [] ->
        cont ()
    | sym :: rest ->
        if List.mem sym rest then err sym else loop rest
  in
  loop symbols

let assert_resolved err = function `Ty ty -> ty | _ -> err `AmbiguousType

let rec typeck_ty delta gamma rho ty err cont =
  match ty with
  | P.VarT "Bool" ->
      cont BoolT
  | P.VarT "Number" ->
      cont NumberT
  | P.VarT "String" ->
      cont StringT
  | P.VarT var ->
      if Delta.mem var delta then failwith "TODO: not yet implemented :("
      else err (`UnboundTyVar var)
  | P.RefT ty ->
      typeck_ty delta gamma rho ty err (fun ty -> cont (Ty.as_ref ty))
  | P.ArrayT ty ->
      typeck_ty delta gamma rho ty err (fun ty' -> cont (ArrayT ty'))
  | P.StructT fields ->
      let rec loop acc = function
        | [] ->
            cont (StructT (List.rev acc))
        | (name, ty) :: rest ->
            typeck_ty delta gamma rho ty err (fun ty' ->
                loop ((Slot.of_string name, ty') :: acc) rest )
      in
      loop [] fields
  | P.FnT (formals, ret_ty, deps, effs) ->
      typeck_ty delta gamma rho ret_ty err (fun ret_ty' ->
          typeck_formals delta gamma rho formals err (fun formals' ->
              let ids, signature_gamma =
                (* FIXME: is this going to be problematic? if gamma is empty then *)
                (* signatures can't reference variables they may capture. *)
                Gamma.extend_many formals' Gamma.empty
              in
              let formals' = List.combine ids (List.map snd formals') in
              let rec convert_dep (P.DepsOn (pl, pls)) k =
                convert_place pl (fun pl ->
                    convert_places pls (fun pls -> k (DepsOn (pl, pls))) )
              and convert_eff (P.WriteE (pls, pl)) k =
                convert_places pls (fun _ ->
                    convert_place pl (fun pl -> k (WriteEf pl)) )
              and convert_places pls k = mapk convert_place pls k
              and convert_deps deps k = mapk convert_dep deps k
              and convert_effs effs k = mapk convert_eff effs k
              and convert_place pl k =
                typeck_place signature_gamma pl err (fun pl _ -> k pl)
              in
              convert_deps deps (fun deps ->
                  convert_effs effs (fun effs ->
                      cont (FnT (Signature.basic ~effs ~deps formals' ret_ty')) ) ) ) )

and typeck_formals delta gamma rho formals err cont =
  let rec loop acc = function
    | [] ->
        cont (List.rev acc)
    | (name, ty) :: rest ->
        typeck_ty delta gamma rho ty err (fun ty' ->
            loop ((name, ty') :: acc) rest )
  in
  check_for_duplicates (List.map fst formals)
    (fun sym -> err (`FormalsDup (formals, sym)))
    (fun () -> loop [] formals)

and typeck_place gamma place err cont =
  let rec inner_place pl cont =
    match pl with
    | P.VarP sym -> (
        Gamma.lookup sym gamma
        |> function
        | `Bound (id, ty) ->
            cont (Place.baseid id) ty
        | `Method (id, _) ->
            cont (Place.baseid id) (MethodGroupT id)
        | `Unbound ->
            err (`UnboundVar sym) )
    | P.DerefP pl ->
        inner_place pl (fun pl ty ->
            match Ty.ref_ty ty with
            | Some ty ->
                cont (Place.slot Slot.box_value pl) ty
            | None ->
                err (`Deref (pl, ty)) )
    | P.SlotP (pl, sl) ->
        inner_place pl (fun pl' ty ->
            match Ty.slot_type ty (Slot.of_string sl) with
            | Some ty' ->
                cont (Place.slot (Slot.of_string sl) pl') ty'
            | None ->
                err (`SlotRef (ty, sl)) )
    | P.ArefP (pl, name) ->
        inner_place pl (fun pl plty ->
            Gamma.lookup name gamma
            |> function
            | `Unbound ->
                err (`UnboundVar name)
            | `Method _ ->
                failwith "NYI: method lookup for place"
            | `Bound (id, ty) ->
                expect NumberT ty
                  (fun () -> err (`ArefNotANumber (place, name)))
                  (fun () ->
                    match (Ty.element_ty plty, int_of_string_opt name) with
                    | None, _ ->
                        err (`ArefNotAnArray (pl, ty))
                    | Some ty', Some n ->
                        cont (Place.offset n pl) ty'
                    | Some ty', None ->
                        cont (Place.dynoffset id pl) ty' ) )
  in
  inner_place place cont

and typeck_expr delta gamma rho expr err cont =
  let basic_ectx ty expr rho deps effs =
    let id = Identifier.fresh () in
    let ectx hole = LetE (id, ty, expr, hole) in
    cont id ty ectx (Gamma.extendid id ty gamma) (Rho.extendid id deps rho) effs
  in
  match expr with
  | P.TrueE ->
      basic_ectx BoolT TrueE rho [] []
  | P.FalseE ->
      basic_ectx BoolT FalseE rho [] []
  | P.LitE n ->
      basic_ectx NumberT (LitE n) rho [] []
  | P.StringE s ->
      basic_ectx StringT (StringE s) rho [] []
  | P.PlaceE pl ->
      typeck_place gamma pl err (fun pl ty ->
          if Place.is_inner pl then basic_ectx ty (PlaceE pl) rho [pl] []
          else
            match Place.get_id_base pl with
            | None ->
                failwith "unreachable"
            | Some id ->
                cont id ty (fun hole -> hole) gamma rho [] )
  | P.IfE (_test, _consequent, _alternate) ->
      failwith "NYI: IfE"
      (* typeck_expr delta gamma rho test err (fun test' ty1 rho deps1 effs1 -> *)
      (*     expect BoolT ty1 *)
      (*       (fun () -> err (`Expr (test, BoolT, ty1))) *)
      (*       (fun () -> *)
      (*         typeck_expr delta gamma rho consequent err *)
      (*           (fun consequent' ty2 rho1 deps2 effs2 -> *)
      (*             typeck_expr delta gamma rho alternate err *)
      (*               (fun alternate' ty3 rho2 deps3 effs3 -> *)
      (*                 expect ty2 ty3 *)
      (*                   (fun () -> err (`Expr (alternate, ty2, ty3))) *)
      (*                   (fun () -> *)
      (*                     cont *)
      (*                       (IfE (test', consequent', alternate')) *)
      (*                       ty2 (Rho.combine rho1 rho2) *)
      (*                       (deps1 @ deps2 @ deps3) *)
      (*                       (effs1 @ effs2 @ effs3) ) ) ) ) ) *)
  | P.BeginE exprs ->
      let rec loop ectx gamma rho effs = function
        | [] ->
            basic_ectx VoidT VoidE rho [] []
        | expr :: [] ->
            typeck_expr delta gamma rho expr err
              (fun id ty ectx' gamma rho effs' ->
                cont id ty
                  (fun hole -> ectx' hole |> ectx)
                  gamma rho (effs @ effs') )
        | pexpr :: rest ->
            typeck_expr delta gamma rho pexpr err
              (fun _ ty ectx gamma rho effs' ->
                expect VoidT ty
                  (fun () -> err (`Expr (pexpr, VoidT, ty)))
                  (fun () -> loop ectx gamma rho (effs @ effs') rest) )
      in
      loop (fun x -> x) gamma rho [] exprs
  | P.StructE fields ->
      let initializers_to_rec ls =
        StructE (List.rev_map (fun (slot, id, _) -> (slot, id)) ls)
      and initializers_to_ty ls =
        StructT (List.rev_map (fun (slot, _, ty) -> (slot, ty)) ls)
      in
      let rec loop acc gamma rho effs ectx = function
        | [] ->
            let structe = initializers_to_rec acc
            and structt = initializers_to_ty acc
            and id = Identifier.fresh () in
            (* Extend rho with field sensitive dependencies: *)
            (*  <record id>.field -> deps *)
            let rho =
              List.fold_right
                (fun (slot, initid, _) rho ->
                  Rho.depson (Place.slotid slot id) initid rho )
                acc rho
            in
            let gamma = Gamma.extendid id structt gamma in
            cont id structt
              (fun hole -> LetE (id, structt, structe, hole) |> ectx)
              gamma rho effs
        | (name, expr) :: rest ->
            typeck_expr delta gamma rho expr err
              (fun id ty ectx' gamma rho effs' ->
                loop
                  ((Slot.of_string name, id, ty) :: acc)
                  gamma rho (effs @ effs')
                  (fun hole -> ectx' hole |> ectx)
                  rest )
      in
      loop [] gamma rho [] (fun x -> x) fields
  | P.FnE (formals, body) ->
      typeck_func delta gamma rho formals body err (fun psig body rho ->
          assert (Signature.is_empty psig) ;
          let ty = FnT psig in
          let classid = Identifier.fresh () in
          let callid = Identifier.fresh () in
          let freevars = Signature.freevars psig in
          let ectx hole =
            LetE
              ( classid
              , UncallableT
              , FnE (psig, body)
              , LetE (callid, ty, NewE (classid, freevars), hole) )
          in
          let gamma =
            Gamma.extendid classid ty gamma |> Gamma.extendid callid ty
          and rho =
            Signature.freevars psig |> List.map Place.baseid
            |> fun pls -> Rho.extendid classid pls rho
          in
          remember_rho classid rho ;
          cont callid ty ectx gamma rho [] )
  | P.AppE (e, exprs) ->
      let typeck_exprs gamma rho exprs cont =
        let rec loop ids tys gamma rho effs ectx = function
          | [] ->
              cont (List.rev ids) (List.rev tys) ectx gamma rho effs
          | expr :: rest ->
              typeck_expr delta gamma rho expr err
                (fun id ty ectx' gamma rho effs' ->
                  loop (id :: ids) (ty :: tys) gamma rho (effs @ effs')
                    (fun hole -> ectx' hole |> ectx)
                    rest )
        in
        loop [] [] gamma rho [] (fun x -> x) exprs
      in
      typeck_expr delta gamma rho e err (fun aid aty aectx gamma rho aeffs ->
          typeck_exprs gamma rho exprs (fun ids tys pectx gamma rho peffs ->
              let resultid = Identifier.fresh () in
              let rho =
                Rho.extendid resultid (List.map Place.baseid (aid :: ids)) rho
              in
              Method.resolve_application gamma aty ids tys err (fun resolve ->
                  let aid, signature =
                    match resolve with
                    | `LambdaApp s ->
                        (aid, s)
                    | `MethodApp m ->
                        (m.id, m.signature)
                  in
                  match Method.instantiate signature ids tys resultid with
                  | Some (returnty, returndeps_subst, returneffs) ->
                      let structid = Identifier.fresh () in
                      (* TODO: add the structy *)
                      let rho =
                        Rho.withdeps (List.map snd returndeps_subst) rho
                      in
                      cont resultid returnty
                        (fun hole ->
                          (* bind the return object *)
                          LetE
                            ( structid
                            , UncallableT
                            , AppE (aid, ids)
                            , LetE
                                ( resultid
                                , returnty
                                , PlaceE
                                    ( Place.baseid structid
                                    |> Place.slot Slot.return )
                                  (* TODO: bind the updating functions *)
                                , hole ) )
                          |> pectx |> aectx )
                        gamma rho
                        (aeffs @ peffs @ returneffs)
                  | None ->
                      err (`UnificationError (signature, ids, tys, resultid)) ) ) )
  | P.SetE (pl, expr) ->
      typeck_place gamma pl err (fun pl setty ->
          typeck_expr delta gamma rho expr err
            (fun argid ty ectx gamma rho effs ->
              match Ty.ref_ty setty with
              | None ->
                  err (`SetNoRef setty)
              | Some innerty ->
                  expect innerty ty
                    (fun () -> err (`Expr (expr, innerty, ty)))
                    (fun () ->
                      let set_id = Identifier.fresh () in
                      let gamma = Gamma.extendid set_id VoidT gamma in
                      (* NOTE: we don't update Rho with the dependence because *)
                      (* the unit value is no "dependent" on the set place or argument *)
                      let written_pl = Place.deref pl in
                      cont set_id VoidT
                        (fun hole ->
                          LetE (set_id, VoidT, SetE (written_pl, argid), hole)
                          |> ectx )
                        gamma rho
                        (WriteEf written_pl :: effs) ) ) )
  | P.RefE expr ->
      typeck_expr delta gamma rho expr err (fun argid ty ectx gamma rho effs ->
          let id = Identifier.fresh () in
          let rho = Rho.depson (Place.derefid id) argid rho in
          let ty = Ty.as_ref ty in
          cont id ty
            (fun hole ->
              LetE (id, ty, StructE [(Slot.box_value, argid)], hole) |> ectx )
            gamma rho effs )

and typeck_func delta gamma rho formals body err cont =
  typeck_formals delta gamma rho formals err (fun formals ->
      let filter_primitives_id =
        List.filter (fun id -> not (Gamma.mem id Gamma.initial))
      in
      let filter_primitives =
        List.filter (fun pl ->
            match Place.get_id_base pl with
            | Some id ->
                not (Gamma.mem id Gamma.initial)
            | None ->
                true )
      in
      let ids, gamma = Gamma.extend_many formals gamma in
      let formals = List.combine ids (List.map snd formals) in
      typeck_body delta gamma body err (fun resultid ectx retty rho effs ->
          let return_dance = StructE [(Slot.return, resultid)] in
          let body = ectx return_dance in
          let freevars = Method.freevars formals body |> filter_primitives_id in
          let keep_formals_and_freevars =
            List.filter (fun pl ->
                match Place.get_id_base pl with
                | Some id ->
                    List.mem id freevars || List.mem id ids
                | None ->
                    false )
          in
          let deps =
            Rho.roots_from (Place.baseid resultid) rho
            |> filter_primitives |> keep_formals_and_freevars
            |> fun pls -> [DepsOn (Place.func_result, pls)]
          in
          let sign = Signature.basic ~deps ~effs ~freevars formals retty in
          cont sign body rho ) )

and typeck_body delta gamma (decls, expr) err cont =
  let typeck_def gamma rho (name, expr) ectx cont =
    typeck_expr delta gamma rho expr err (fun id ty ectx' gamma rho effs ->
        let nameid, gamma = Gamma.extend name ty gamma in
        let rho = Rho.depsonid nameid id rho in
        cont
          (fun hole ->
            LetE (nameid, ty, PlaceE (Place.baseid id), hole) |> ectx' |> ectx
            )
          gamma rho effs )
  in
  let typeck_defn gamma rho (name, formals, body) ectx cont =
    typeck_func delta gamma rho formals body err (fun sign body rho ->
        let classid, methd, gamma = Gamma.add_method name sign gamma in
        let instanceid = methd.id in
        let fnty = FnT methd.signature and fne = FnE (methd.signature, body) in
        let freevars = Signature.freevars methd.signature in
        let ectx hole =
          LetE
            ( classid
            , UncallableT
            , fne
            , LetE (instanceid, fnty, NewE (classid, freevars), hole) )
          |> ectx
        in
        remember_rho classid rho ; cont ectx gamma )
  in
  let typeck_decls gamma rho decls cont =
    let rec loop gamma rho effs ectx = function
      | [] ->
          cont ectx gamma rho effs
      | P.Def (s, e) :: rest ->
          typeck_def gamma rho (s, e) ectx (fun ectx' gamma rho effs' ->
              loop gamma rho (effs @ effs') ectx' rest )
      | P.Defn (s, fs, b) :: rest ->
          typeck_defn gamma rho (s, fs, b) ectx (fun ectx' gamma ->
              loop gamma rho effs ectx' rest )
    in
    loop gamma rho [] (fun x -> x) decls
  in
  typeck_decls gamma Rho.initial decls (fun defectx gamma rho defeffs ->
      typeck_expr delta gamma rho expr err (fun bodyid ty ectx _ rho effs ->
          let bodyectx hole = hole |> ectx |> defectx in
          cont bodyid bodyectx ty rho (effs @ defeffs) ) )

let report_type_error err =
  let open Sexplib.Sexp in
  let sexp s = output_hum stderr s in
  let nl () = output_string stderr "\n" in
  let string s = output_string stderr s in
  let num n = output_string stderr (string_of_int n) in
  let arrow tys =
    List.iteri
      (fun i ty ->
        if i > 0 then string " -> " ;
        sexp (sexp_of_ty ty) )
      tys
  in
  let printit () = flush stderr in
  string "~~~ Type Error ~~~\n" ;
  ( match err with
  | `UnboundVar sym ->
      string "Unbound variable " ; string sym
  | `UnboundTyVar sym ->
      string "Unbound type variable " ;
      string sym
  | `FormalsDup (_, sym) ->
      string "Duplicate formal argument " ;
      string sym
  | `BodyDup (_, sym) ->
      string "Duplicate definition " ;
      string sym
  | `Expr (expr, ex, ac) ->
      string "Expected expression: " ;
      nl () ;
      sexp (P.sexp_of_expr expr) ;
      nl () ;
      string "to have type: " ;
      sexp (sexp_of_ty ex) ;
      nl () ;
      string "but got: " ;
      sexp (sexp_of_ty ac)
  | `SetNoRef ty ->
      string "Cannot `set!` the non-reference type" ;
      sexp (sexp_of_ty ty)
  | `Deref (pl, ty) ->
      string "Cannot dereference the non-reference type " ;
      sexp (sexp_of_ty ty) ;
      nl () ;
      string "of place: " ;
      sexp (Place.sexp_of_t pl)
  | `AppNotFn ty ->
      string "Expected a callable type" ;
      nl () ;
      string "but got: " ;
      sexp (sexp_of_ty ty)
  | `NoMethodApplicable (ms, tys) ->
      string "Could not find applicable method for arguments:" ;
      nl () ;
      arrow tys ;
      nl () ;
      nl () ;
      string "Available methods:" ;
      nl () ;
      List.iter
        (fun m ->
          sexp (sexp_of_polysig m.signature) ;
          nl () )
        ms
  | `AmbiguousMethodApp (ms, tys) ->
      string "Ambiguous method application for " ;
      nl () ;
      arrow tys ;
      nl () ;
      nl () ;
      string "Ambiguous choices:" ;
      List.iter
        (fun m ->
          sexp (sexp_of_polysig m.signature) ;
          nl () )
        ms
  | `SlotRef (ty, sl) ->
      string "Type " ;
      sexp (sexp_of_ty ty) ;
      nl () ;
      string " has no slot " ;
      string sl
  | `ArefNotAnArray (pl, ty) ->
      string "Cannot index non-array type" ;
      nl () ;
      sexp (Place.sexp_of_t pl) ;
      nl () ;
      string "has type: " ;
      sexp (sexp_of_ty ty)
  | `ArefNotANumber (pl, name) ->
      string "Cannot index array with non-number " ;
      sexp (P.sexp_of_place pl) ;
      nl () ;
      string " slot " ;
      string name ;
      string " is not a number"
  | `ArgumentMismatch (i, exp, act) ->
      string "Argument " ;
      num i ;
      string " expected type " ;
      nl () ;
      sexp (sexp_of_ty exp) ;
      nl () ;
      nl () ;
      string "but got " ;
      nl () ;
      sexp (sexp_of_ty act)
  | `ArityMismatch (exp, act) ->
      string "Expected " ;
      num exp ;
      string " arguments but got " ;
      num act
  | `UnificationError (signature, ids, tys, _) ->
      (* TODO *)
      string "Unification error: " ;
      nl () ;
      string "Signature: " ;
      sexp (sexp_of_polysig signature) ;
      sexp (sexp_of_ty_bindings (List.combine ids tys))
  | `BadApplication ->
      string "Bad application"
  | _ ->
      failwith "NYI: typeck report_error" )
  |> nl |> printit
  |> fun () -> failwith "typeck error"

let run (prog : P.program) : program =
  typeck_body Delta.initial Gamma.initial prog report_type_error
    (fun resultid ectx ty _ _ ->
      let expr = ectx (PlaceE (Place.baseid resultid)) in
      {expr; ty; rhos= global_ctx ()} )

let sexp_of_program = sexp_of_program

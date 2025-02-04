open Env
open Lang
open Ant.Ty
open Ant.Util
open ResultMonad

let rec infer_ty st = function
  | Parse.VarT "Bool" ->
      return Ty.bool
  | Parse.VarT "Number" ->
      return Ty.number
  | Parse.VarT "String" ->
      return Ty.string
  | Parse.VarT var ->
      error (`Msg ("TODO: VarT " ^ var))
  | Parse.RefT ty ->
      let+ ty = infer_ty st ty in
      Ty.ref ty
  | Parse.ArrayT ty ->
      let+ ty = infer_ty st ty in
      Ty.array ty
  | Parse.StructT fields ->
      let rec loop acc = function
        | [] ->
            List.rev acc |> Ty.struct_ |> return
        | (name, ty) :: rest ->
            let* ty = infer_ty st ty in
            loop ((Slot.of_string name, ty) :: acc) rest
      in
      loop [] fields
  | Parse.FnT (formals, ret_ty, deps, effs) ->
      let rec loop acc = function
        | [] ->
            List.rev acc |> return
        | (name, ty) :: rest ->
            let* ty = infer_ty st ty in
            loop ((Symbol.var name, ty) :: acc) rest
      in
      let* formals = loop [] formals in
      let* retty = infer_ty st ret_ty in
      (* TODO: we need to bind the arguments here before checking the deps/effs *)
      let* deps = infer_deps st deps in
      let+ effs = infer_effs st effs in
      Ty.arrow ~deps ~effs formals retty

and infer_deps _st _deps = failwith "NYI: deps"

and infer_effs _st _deps = failwith "NYI: effs"

and infer_symbol sa sv sym : (Symbol.t * Ty.t, 'e) result =
  let* id = Alpha.lookup sym sa in
  let+ ty = Gamma.lookup id sv in
  (* If `id` is bound to a method group with a single *)
  (* overload, we just return the overload as if it were a *)
  (* lambda. *)
  let id, ty =
    match Gamma.as_single_overload ty sv with
    | Ok (id, ty) ->
        (id, ty)
    | Error _ ->
        (id, ty)
  in
  (id, Ty.instantiate ty)

and infer_place sa sv place =
  let rec inner_place pl : (Place.t * Ty.t, 'e) result =
    match pl with
    | Parse.VarP sym ->
        let+ id, ty = infer_symbol sa sv sym in
        (Place.baseid id, ty)
    | Parse.DerefP pl ->
        let inner_ty = Ty.new_meta () in
        let refty = Ty.ref inner_ty in
        let* pl, ty = inner_place pl in
        let+ () = Ty.unify refty ty in
        (Place.deref pl, inner_ty)
    | Parse.SlotP (pl, sl) ->
        let* pl, ty = inner_place pl in
        let sl = Slot.of_string sl in
        let+ slotty = Ty.lookup_slot sl ty in
        (Place.slot sl pl, slotty)
    | Parse.ArefP (pl, name) -> (
        let inner_ty = Ty.new_meta () in
        let arrty = Ty.array inner_ty in
        let* pl, ty = inner_place pl in
        let* () = Ty.unify arrty ty in
        match int_of_string_opt name with
        | Some n ->
            return (Place.offset n pl, inner_ty)
        | None ->
            let* id, ty = infer_symbol sa sv name in
            let+ () = Ty.unify Ty.number ty in
            (Place.dynoffset id pl, inner_ty) )
  in
  inner_place place

and infer_expr sa sv st = function
  | Parse.TrueE ->
      return (TrueE, Ty.bool, Dependencies.empty, Effects.empty)
  | Parse.FalseE ->
      return (FalseE, Ty.bool, Dependencies.empty, Effects.empty)
  | Parse.LitE n ->
      return (LitE n, Ty.number, Dependencies.empty, Effects.empty)
  | Parse.StringE s ->
      return (StringE s, Ty.string, Dependencies.empty, Effects.empty)
  | Parse.PlaceE pl ->
      let+ pl, ty = infer_place sa sv pl in
      let deps =
        if Gamma.is_primitive pl then Dependencies.empty
        else Dependencies.singleton (Dep.deps [pl])
      in
      (PlaceE pl, ty, deps, Effects.empty)
  | Parse.IfE (test, consequent, alternate) ->
      let* test, tty, tdeps, teffs = infer_expr sa sv st test in
      let* () = Ty.unify Ty.bool tty in
      let* consequent, cty, cdeps, ceffs = infer_expr sa sv st consequent in
      let* alternate, aty, adeps, aeffs = infer_expr sa sv st alternate in
      let+ () = Ty.unify cty aty in
      ( IfE (test, consequent, alternate)
      , cty
      , Dependencies.(tdeps @ cdeps @ adeps)
      , Effects.(teffs @ ceffs @ aeffs) )
  | Parse.BeginE exprs ->
      let rec loop effs acc = function
        | [] ->
            return (VoidE, Ty.void, Dependencies.empty, effs)
        | [e] ->
            let id = Symbol.fresh () in
            let* e, ty, deps, effs' = infer_expr sa sv st e in
            let+ expr =
              List.fold_right
                (fun (e, ty) acc ->
                  let* body = acc in
                  let+ () = Ty.unify Ty.void ty in
                  let id = Symbol.fresh () in
                  LetE
                    { bound= id
                    ; ty= Ty.void
                    ; deps= Dependencies.empty
                    ; expr= e
                    ; body } )
                acc
                ( LetE
                    { bound= id
                    ; ty
                    ; deps
                    ; expr= e
                    ; body= PlaceE (Place.baseid id) }
                |> return )
            in
            (expr, ty, deps, Effects.(effs @ effs'))
        | e :: exprs ->
            let* e, ty, deps, effs' = infer_expr sa sv st e in
            Logs.debug (fun m ->
                m "within %a:@ %a@.%a@." pp_expr e Dependencies.pp deps
                  Effects.pp effs' ) ;
            (* NOTE, we can't assert that it's empty here because a set may *)
            (* have a function name in it...there is something wrong here, but *)
            (* I'm still not sure what. *)
            (* let* () = Dependencies.assert_empty deps in *)
            ignore deps ;
            loop Effects.(effs @ effs') ((e, ty) :: acc) exprs
      in
      loop Effects.empty [] exprs
  | Parse.StructE fields ->
      let rec loop deps effs acc = function
        | [] ->
            let structe =
              StructE (List.rev_map (fun (s, _, e) -> (s, e)) acc)
            in
            let structt =
              Ty.struct_ (List.rev_map (fun (s, t, _) -> (s, t)) acc)
            in
            return (structe, structt, deps, effs)
        | (slot, e) :: rest ->
            let* e, ty, edeps, effs' = infer_expr sa sv st e in
            let slot = Slot.of_string slot in
            let fdeps =
              Dependencies.adjust_lhs
                (fun pl -> if Place.is_hole pl then Place.slot slot pl else pl)
                edeps
            in
            loop
              Dependencies.(deps @ fdeps)
              Effects.(effs @ effs')
              ((slot, ty, e) :: acc) rest
      in
      loop Dependencies.empty Effects.empty [] fields
  | Parse.SetE (pl, e) ->
      let inner_ty = Ty.new_meta () in
      let refty = Ty.ref inner_ty in
      let* pl, plty = infer_place sa sv pl in
      let* e, ety, deps, effs = infer_expr sa sv st e in
      let deps = Dependencies.adjust_lhs (Place.plug pl) deps in
      let effs = Effects.(effs ++ deps) in
      let* () = Ty.unify inner_ty ety in
      let+ () = Ty.unify refty plty in
      (* FIXME: is this correct??? *)
      (SetE (pl, e), Ty.void, Dependencies.empty, effs)
  | Parse.RefE e ->
      let+ e, ty, deps, effs = infer_expr sa sv st e in
      let deps = Dependencies.adjust_lhs Place.deref deps in
      (RefE e, Ty.ref ty, deps, effs)
  | Parse.FnE (formals, body) ->
      let args, tys = List.split formals in
      let+ s, body = infer_func sa sv st args tys body in
      (FnE (s, body), Signature.to_ty s, Dependencies.empty, Effects.empty)
  | Parse.AppE [] ->
      error (`Msg "ICE: empty application")
  | Parse.AppE (s :: ss) ->
      let* e, ety = infer_place sa sv s in
      let* args = List.all (fun s -> infer_place sa sv s) ss in
      let args, tys = List.split args in
      (* `ety` could represent a method group, we need to resolve the specific application *)
      let* f, fty =
        match Ty.as_group ety with
        | Ok group ->
            let+ id, ety =
              Signature.resolve_application (Gamma.methods group sv) tys
            in
            (Place.baseid id, ety)
        | Error _ ->
            return (e, ety)
      in
      let* deps = Signature.dependencies args fty in
      let* effs = Signature.effects args fty in
      let+ retty = Signature.return fty in
      (* let deps = Dependencies.(deps @ edeps) in *)
      (* let effs = List.fold_left Effects.merge eeffs (reffs :: effss) in *)
      (AppE {tyargs= tys; f; args}, retty, deps, effs)

and infer_func ?(tempbind = fun _ g -> g) sa sv st pargs tys body =
  Logs.debug (fun m ->
      m "infer_func: @[(%a)%a@]"
        (Format.pp_print_list Parse.pp_ty_binding)
        (List.combine pargs tys) Parse.pp_program body ) ;
  let args, sa = Alpha.extend' pargs sa in
  (* Get a meta version of the signature  *)
  let mtys = List.map (fun _ -> Ty.new_meta ()) tys in
  let mret = Ty.new_meta () in
  let effs = Ty.new_meta () in
  let deps = Ty.new_meta () in
  (* check the annotated types *)
  let* tys =
    List.all2
      (fun ty mty ->
        let* ty = infer_ty st ty in
        let+ () = Ty.unify ty mty in
        ty )
      tys mtys
  in
  let sv = Gamma.extend (List.combine args tys) sv in
  (* use `func` to get an instantiated AppT type *)
  let msig = Ty.func ~deps ~effs (List.combine args mtys) mret in
  (* The caller can choose to bind the signature to the name in the body *)
  (* We wrap the signature in an empty `PolyT` variant as a *)
  (* "monomorphized" function, but the final signature will get generalized *)
  (* after. *)
  let sv = tempbind (PolyT ([], msig)) sv in
  let* body, ret, bdeps, beffs = infer_body sa sv st body in
  let* () = Ty.unify mret ret in
  (* TODO: we need to pack the dependencies for signature use. This *)
  (* means the holes are plugged with the `resultb` value, but it *)
  (* also means that for each inner `ref` that's leaked, an existential *)
  (* is introduced. *)
  let bdeps = Dependencies.adjust_lhs (Place.plug Place.result) bdeps in
  let* () = Ty.unify deps (DepsT bdeps) in
  let* () = Ty.unify effs (EffsT beffs) in
  let msig = Ty.generalize (Gamma.ty_subst sv) msig in
  let+ sg = Signature.from_ty msig in
  (sg, body)

and infer_def sa sv st (name, expr) =
  Logs.debug (fun m -> m "infer_def %s" name) ;
  let+ expr, ty, deps, effs = infer_expr sa sv st expr in
  (* NOTE: the current implementation only supports *)
  (* rank-1 polymorphism. To this extent, we don't generalize *)
  (* types at variable declaration, though we might want to *)
  (* consider it. *)
  let id, sa = Alpha.insert' name sa in
  let deps = Dependencies.adjust_lhs (Place.plug (Place.baseid id)) deps in
  let sv = Gamma.insert id ty sv in
  (sa, sv, `Let (id, ty, expr, deps, effs))

and infer_defn sa sv st (name, formals, body) =
  (* NOTE, there's a bit of an awkward dance here where *)
  (* we want to add the definition `name` as a new member *)
  (* of the method group. Because the `tempbind` callback *)
  (* handles that, we need to make sure the resolved id in the body *)
  (* is the same as that bound in the `LetRecE`. Therefore, we create *)
  (* our own `overloadid` and tie all the expressions together. *)
  Logs.debug (fun m -> m "infer_defn %s" name) ;
  let args, tys = List.split formals in
  let id, sa = Alpha.lookup_or_insert name sa in
  let overloadid = Symbol.derivative id in
  let tempbind ty sv =
    Gamma.add_method id ~overloadid ty sv |> Result.unwrap |> snd
  in
  let* s, body = infer_func ~tempbind sa sv st args tys body in
  (* bind the ty as a method *)
  let ty = Signature.to_ty s in
  let+ _, sv = Gamma.add_method id ~overloadid ty sv in
  (sa, sv, `Label (overloadid, ty, FnE (s, body)))

and infer_body sa sv st (decls, expr) =
  let* defs, sa, sv =
    List.fold_left
      (fun acc decl ->
        let* acc, sa, sv = acc in
        match decl with
        | Parse.Def (name, expr) ->
            let+ sa, sv, th = infer_def sa sv st (name, expr) in
            (th :: acc, sa, sv)
        | Parse.Defn (name, formals, body) ->
            let+ sa, sv, th = infer_defn sa sv st (name, formals, body) in
            (th :: acc, sa, sv) )
      (return ([], sa, sv))
      decls
  in
  let+ expr, ty, deps, effs = infer_expr sa sv st expr in
  let deps, effs, expr =
    List.fold_left
      (fun (deps, effs, body) -> function
        | `Let (id, ty, e, ds, es) ->
            Logs.debug (fun m ->
                m "%a %a @[%a@. %a@.@]@." Symbol.pp id pp_expr e Dependencies.pp
                  ds Effects.pp es ) ;
            let deps = Dependencies.purge id ds deps in
            let effs = Effects.remove id effs |> Effects.merge es in
            (deps, effs, LetE {bound= id; ty; deps= ds; expr= e; body})
        | `Label (id, ty, e) ->
            let deps = Dependencies.purge id Dependencies.empty deps in
            (deps, effs, LetRecE (id, ty, e, body)) )
      (deps, effs, expr) defs
  in
  (expr, ty, deps, effs)

let report_type_error (`Msg str) =
  Format.eprintf "Type error: %s@." str ;
  exit 1

let run (prog : Parse.program) : program =
  let run_inner () =
    let* body, ty, deps, effs =
      infer_body Alpha.initial Gamma.initial Theta.initial prog
    in
    ignore (deps, effs) ;
    (* TODO *)
    (* let* () = Dependencies.assert_empty deps in *)
    let+ () = Ty.unify Ty.void ty in
    body
  in
  run_inner ()
  |> function Ok body -> body | Error err -> report_type_error err

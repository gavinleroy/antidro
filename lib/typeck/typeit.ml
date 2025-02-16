open Env
open Lang
open Ant.Ty
open Ant.Util
open ResultMonad

(* and typeit_deps _st _deps = failwith "NYI: deps" *)

(* and typeit_effs _st _deps = failwith "NYI: effs" *)

let rec typeit_symbol sv id : (Symbol.t * Ty.t, 'e) result =
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

and typeit_place sv pl : (Place.t * Ty.t, 'e) result =
  let rec loop acc = function
    | [] ->
        return acc
    | adj :: rest -> (
      match adj with
      | Place.SlotAdj slot ->
          let* slot_ty = Ty.lookup_slot slot acc in
          loop slot_ty rest
      | Place.OffAdj _ ->
          let inner = Ty.new_meta () in
          let* () = Ty.unify acc (Ty.array inner) in
          loop inner rest )
  in
  match Place.get_id_base pl with
  | Ok id ->
      let* id, ty = typeit_symbol sv id in
      let inner, rewrap =
        match Ty.as_ref_inner_ty ty with
        | Some inner ->
            (inner, Ty.ref)
        | None ->
            (ty, fun x -> x)
      in
      let+ ty = loop inner (Place.get_adjustments pl) in
      (Place.replace_inner (Place.baseid id) pl, rewrap ty)
  | Error _ ->
      Result.error "not a concrete place, cannot type: %a" Place.pp pl

and typeit_expr sv st = function
  | VoidE ->
      return (VoidE, Ty.void, Dependencies.empty, Effects.empty)
  | TrueE ->
      return (TrueE, Ty.bool, Dependencies.empty, Effects.empty)
  | FalseE ->
      return (FalseE, Ty.bool, Dependencies.empty, Effects.empty)
  | LitE n ->
      return (LitE n, Ty.number, Dependencies.empty, Effects.empty)
  | StringE s ->
      return (StringE s, Ty.string, Dependencies.empty, Effects.empty)
  | PlaceE pl ->
      let+ pl, ty = typeit_place sv pl in
      let deps =
        if Gamma.is_primitive pl then Dependencies.empty
        else Dependencies.singleton (Dep.deps [pl])
      in
      (PlaceE pl, ty, deps, Effects.empty)
  | IfE (test, consequent, alternate) ->
      let* test, tty, tdeps, teffs = typeit_expr sv st test in
      let* () = Ty.unify Ty.bool tty in
      let* consequent, cty, cdeps, ceffs = typeit_expr sv st consequent in
      let* alternate, aty, adeps, aeffs = typeit_expr sv st alternate in
      let+ () = Ty.unify cty aty in
      ( IfE (test, consequent, alternate)
      , cty
      , Dependencies.(tdeps @ cdeps @ adeps)
      , Effects.(teffs @ ceffs @ aeffs) )
  | LetE {bound= id; expr; body; _} ->
      let* expr, ty, deps, effs = typeit_expr sv st expr in
      let deps = Dependencies.adjust_lhs (Place.plug (Place.baseid id)) deps in
      let+ body, bodyty, bdeps, beffs =
        typeit_expr (Gamma.insert id ty sv) st body
      in
      let expr = LetE {bound= id; ty; (* deps; *) expr; body} in
      let deps = Dependencies.purge id bdeps deps in
      let effs = Effects.remove id effs |> Effects.merge beffs in
      (expr, bodyty, deps, effs)
  | LabelE {groupid; overloadid; args; fbody; body} ->
      Logs.debug (fun m -> m "typeit_label: %a" Symbol.pp overloadid) ;
      let mret = Ty.new_meta () in
      let ty = Ty.arrow args mret in
      let* _, sv = Gamma.add_method ~overloadid groupid ty sv in
      let* s, fbody = typeit_func sv st args fbody in
      Logs.debug (fun m ->
          m "unifying label signatures for %a %a" Symbol.pp overloadid
            Signature.pp s ) ;
      let* () = Ty.unify ty (Signature.to_ty s) in
      let+ body, bodyty, bdeps, beffs = typeit_expr sv st body in
      let expr = LabelE {groupid; overloadid; args; fbody; body} in
      (expr, bodyty, bdeps, beffs)
  | StructE fields ->
      let rec loop deps acc = function
        | [] ->
            let structe =
              StructE (List.rev_map (fun (s, _, e) -> (s, e)) acc)
            in
            let structt =
              Ty.struct_ (List.rev_map (fun (s, t, _) -> (s, t)) acc)
            in
            return (structe, structt, deps, Effects.empty)
        | (slot, e) :: rest ->
            let* e, ty = typeit_symbol sv e in
            let fdeps =
              Dep.on (Place.slot slot Place.hole) [Place.baseid e]
              |> Dependencies.singleton
            in
            loop Dependencies.(deps @ fdeps) ((slot, ty, e) :: acc) rest
      in
      loop Dependencies.empty [] fields
  | SetE (pl, e) ->
      let inner_ty = Ty.new_meta () in
      let refty = Ty.ref inner_ty in
      let* pl, plty = typeit_place sv pl in
      let* e, ety = typeit_symbol sv e in
      (* TODO: should we return the dependencies here too? *)
      let effs = Effects.singleton (Eff.on pl [Place.baseid e]) in
      let* () = Ty.unify inner_ty ety in
      let+ () = Ty.unify refty plty in
      (SetE (pl, e), Ty.void, Dependencies.empty, effs)
  | RefE e ->
      (* FIXME: after getting rid of the `DerefAdj` we put dependencies on the raw place *)
      let+ e, ty = typeit_symbol sv e in
      let deps = Dependencies.singleton (Dep.on Place.hole [Place.baseid e]) in
      (RefE e, Ty.ref ty, deps, Effects.empty)
  | DerefE e ->
      let inner_ty = Ty.new_meta () in
      let* e, ty = typeit_symbol sv e in
      let+ () = Ty.unify ty (Ty.ref inner_ty) in
      let deps = Dependencies.singleton (Dep.on Place.hole [Place.baseid e]) in
      (DerefE e, inner_ty, deps, Effects.empty)
  | FnE (formals, body) ->
      let+ s, body = typeit_func sv st formals body in
      (FnE (formals, body), Signature.to_ty s, Dependencies.empty, Effects.empty)
  | AppE {f; args; _} ->
      let* f, fty = typeit_place sv f in
      Logs.debug (fun m ->
          m "Typing application of %a / %a" Place.pp f Ty.pp fty ) ;
      let* formals = List.all (fun s -> typeit_symbol sv s) args in
      let args, tys = List.split formals in
      let* f, fty =
        match Ty.as_group fty with
        | Ok group ->
            let+ id, fty =
              Signature.resolve_application (Gamma.methods group sv) tys
            in
            (Place.baseid id, fty)
        | Error _ ->
            return (f, fty)
      in
      let* deps = Signature.dependencies (List.map Place.baseid args) fty in
      let* effs = Signature.effects (List.map Place.baseid args) fty in
      let* retty = Signature.return fty in
      let raw_fun =
        Ty.func (List.map (fun t -> (Symbol.fresh (), t)) tys) retty
      in
      let+ () = Ty.unify fty raw_fun in
      Logs.debug (fun m -> m "I did it!") ;
      (AppE {f; args}, retty, deps, effs)

and typeit_func ?(tempbind = fun _ g -> g) sv st (formals : formals) body =
  Logs.debug (fun m ->
      m "typeit_func: @[(%a)%a@]"
        (Format.pp_print_list (fun fmt (id, ty) ->
             Format.fprintf fmt "%a: %a" Symbol.pp id Ty.pp ty ) )
        formals pp_expr body ) ;
  (* Build a meta version of the signature  *)
  let tys = List.map snd formals in
  let mtys = List.map (fun _ -> Ty.new_meta ()) tys in
  let mret = Ty.new_meta () in
  let effs = Ty.new_meta () in
  let deps = Ty.new_meta () in
  (* check the annotated types *)
  let* _ = List.all2 Ty.unify tys mtys in
  let sv = Gamma.extend formals sv in
  (* use `func` to get an instantiated AppT type *)
  let msig = Ty.func ~deps ~effs formals mret in
  (* The caller can choose to bind the signature to the name in the body *)
  (* We wrap the signature in an empty `PolyT` variant as a *)
  (* "monomorphized" function, but the final signature will get generalized *)
  (* after. *)
  let sv = tempbind (PolyT ([], msig)) sv in
  let* body, ret, bdeps, beffs = typeit_expr sv st body in
  let* () = Ty.unify mret ret in
  (* TODO: we need to pack the dependencies for signature use. This *)
  (* means the holes are plugged with the `resultb` value, but it *)
  (* also means that for each inner `ref` that's leaked, an existential *)
  (* is introduced. *)
  let bdeps = Dependencies.adjust_lhs (Place.plug Place.return) bdeps in
  let* () = Ty.unify deps (Dependencies.to_ty bdeps) in
  let* () = Ty.unify effs (Effects.to_ty beffs) in
  Logs.debug (fun m -> m "Inferred signature ret %a %a" Ty.pp ret Ty.pp msig) ;
  let msig = Ty.generalize (Gamma.ty_subst sv) msig in
  let+ sg = Signature.from_ty msig in
  (sg, body)

let report_type_error err =
  Format.eprintf "Type error: @[<v 0>%a@]@." Error.pp err ;
  exit 1

let run (prog : program) : program =
  let run () : (program, Error.t) result =
    let+ body, _ty, _deps, _effs =
      typeit_expr Gamma.initial Theta.initial prog
    in
    body
  in
  run () |> function Ok prog -> prog | Error err -> report_type_error err

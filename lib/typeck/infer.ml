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
      let* deps = infer_deps st deps in
      let+ effs = infer_effs st effs in
      Ty.arrow ~deps ~effs formals retty

and infer_deps _st _deps = failwith "NYI: deps"

and infer_effs _st _deps = failwith "NYI: effs"

and infer_place sa sv place =
  let rec inner_place pl =
    match pl with
    | Parse.VarP sym ->
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
        (Place.baseid id, ty |> Ty.instantiate)
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
            let* id = Alpha.lookup name sa in
            let* ty = Gamma.lookup id sv in
            let ty = Ty.instantiate ty in
            let+ () = Ty.unify Ty.number ty in
            (Place.dynoffset id pl, inner_ty) )
  in
  inner_place place

and infer_expr sa sv st = function
  | Parse.TrueE ->
      return (TrueE, Ty.bool)
  | Parse.FalseE ->
      return (FalseE, Ty.bool)
  | Parse.LitE n ->
      return (LitE n, Ty.number)
  | Parse.StringE s ->
      return (StringE s, Ty.string)
  | Parse.PlaceE pl ->
      let+ pl, ty = infer_place sa sv pl in
      (PlaceE pl, ty)
  | Parse.IfE (test, consequent, alternate) ->
      let* test, tty = infer_expr sa sv st test in
      let* () = Ty.unify Ty.bool tty in
      let* consequent, cty = infer_expr sa sv st consequent in
      let* alternate, aty = infer_expr sa sv st alternate in
      let+ () = Ty.unify cty aty in
      (IfE (test, consequent, alternate), cty)
  | Parse.BeginE exprs ->
      let rec loop acc = function
        | [] ->
            return (VoidE, Ty.void)
        | [e] ->
            let id = Symbol.fresh () in
            let* e, ty = infer_expr sa sv st e in
            let+ expr =
              List.fold_right
                (fun (e, ty) acc ->
                  let* acc = acc in
                  let+ () = Ty.unify Ty.void ty in
                  let id = Symbol.fresh () in
                  LetE (id, Ty.void, e, acc) )
                acc
                (LetE (id, ty, e, PlaceE (Place.baseid id)) |> return)
            in
            (expr, ty)
        | e :: exprs ->
            let* e, ty = infer_expr sa sv st e in
            loop ((e, ty) :: acc) exprs
      in
      loop [] exprs
  | Parse.StructE fields ->
      let rec loop acc = function
        | [] ->
            let structe = StructE (List.rev acc) in
            let structt =
              Ty.struct_ (List.rev_map (fun (s, t, _) -> (s, t)) acc)
            in
            return (structe, structt)
        | (slot, e) :: rest ->
            let* e, ty = infer_expr sa sv st e in
            loop ((Slot.of_string slot, ty, e) :: acc) rest
      in
      loop [] fields
  | Parse.SetE (pl, e) ->
      let inner_ty = Ty.new_meta () in
      let refty = Ty.ref inner_ty in
      let* pl, plty = infer_place sa sv pl in
      let* e, ety = infer_expr sa sv st e in
      let* () = Ty.unify inner_ty ety in
      let+ () = Ty.unify refty plty in
      (SetE (pl, e), Ty.void)
  | Parse.RefE e ->
      let+ e, ty = infer_expr sa sv st e in
      (RefE e, Ty.ref ty)
  | Parse.FnE (formals, body) ->
      let args, tys = List.split formals in
      let+ s, body = infer_func sa sv st args tys body in
      (FnE (s, body), Signature.to_ty s)
  | Parse.AppE (e, exprs) ->
      let* e, ety = infer_expr sa sv st e in
      let* args = List.all (fun e -> infer_expr sa sv st e) exprs in
      let args, tys = List.split args in
      (* `ety` could represent a method group, we need to resolve the specific application *)
      let* f, fty =
        match Ty.as_group ety with
        | Ok group ->
            let+ id, ety =
              Ty.resolve_application (Gamma.methods group sv) tys
            in
            (Resolved id, ety)
        | Error _ ->
            return (Anon e, ety)
      in
      let+ retty = Signature.return fty in
      (AppE {fty; tyargs= tys; f; args}, retty)

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
  let* body, ret = infer_body sa sv st body in
  let* () = Ty.unify mret ret in
  let msig = Ty.generalize (Gamma.ty_subst sv) msig in
  let+ sg = Signature.from_ty msig in
  (sg, body)

and infer_def sa sv st (name, expr) =
  Logs.debug (fun m -> m "infer_def %s" name) ;
  let+ expr, ty = infer_expr sa sv st expr in
  (* TODO: the current implementation only supports *)
  (* rank-1 polymorphism. To this extent, we don't generalize *)
  (* types at variable declaration, though we might want to *)
  (* consider it. (Not needed now) *)
  let id, sa = Alpha.insert' name sa in
  let sv = Gamma.insert id ty sv in
  (sa, sv, `Let (id, ty, expr))

and infer_defn sa sv st (name, formals, body) =
  (* NOTE, there's a bit of an awkward dance here where *)
  (* we want to add the definition `name` as a new member *)
  (* of the method group. Because the `tempbind` callback *)
  (* handles that, we need to make sure the resolved id in the body *)
  (* is the same as that bound in the `LetRecE`. Because of this *)
  (* we create our own `overloadid` and tie all the expressions *)
  (* together. *)
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
  let+ expr, ty = infer_expr sa sv st expr in
  ( List.fold_left
      (fun body -> function
        | `Let (id, ty, e) ->
            LetE (id, ty, e, body)
        | `Label (id, ty, e) ->
            LetRecE (id, ty, e, body) )
      expr defs
  , ty )

let report_type_error (`Msg str) =
  Format.eprintf "Type error: %s@." str ;
  exit 1

let run (prog : Parse.program) : program =
  let run_inner () =
    let* body, ty = infer_body Alpha.initial Gamma.initial Theta.initial prog in
    let+ () = Ty.unify Ty.void ty in
    body
  in
  run_inner ()
  |> function Ok body -> body | Error err -> report_type_error err

open Env
open Lang
open Ant.Ty
open Ant.Util

let ecompose ectx1 ectx2 hole = ectx1 (ectx2 hole)

let rec simplify_ty (ty : Parse.ty) (error : 'e -> expr) (k : Ty.t -> expr) =
  match ty with
  | Parse.VarT "Bool" ->
      k Ty.bool
  | Parse.VarT "Number" ->
      k Ty.number
  | Parse.VarT "String" ->
      k Ty.string
  | Parse.VarT var ->
      error (`Msg ("TODO: VarT " ^ var))
  | Parse.RefT ty ->
      simplify_ty ty error @@ fun ty -> k (Ty.ref ty)
  | Parse.ArrayT ty ->
      simplify_ty ty error @@ fun ty -> k (Ty.array ty)
  | Parse.StructT fields ->
      let rec loop acc = function
        | [] ->
            k (Ty.struct_ (List.rev acc))
        | (name, ty) :: rest ->
            simplify_ty ty error
            @@ fun ty -> loop ((Slot.of_string name, ty) :: acc) rest
      in
      loop [] fields
  | Parse.FnT (formals, ret_ty, deps, effs) ->
      let rec loop k acc = function
        | [] ->
            k (List.rev acc)
        | (name, ty) :: rest ->
            simplify_ty ty error
            @@ fun ty -> loop k ((Symbol.var name, ty) :: acc) rest
      in
      loop
        (fun formals ->
          simplify_ty ret_ty error
          @@ fun retty ->
          simplify_deps deps
          @@ fun deps ->
          simplify_effs effs
          @@ fun effs -> k (Ty.arrow ~deps ~effs formals retty) )
        [] formals

and simplify_tys tys error k =
  let rec loop acc = function
    | [] ->
        k (List.rev acc)
    | ty :: rest ->
        simplify_ty ty error @@ fun ty -> loop (ty :: acc) rest
  in
  loop [] tys

and simplify_deps _deps _k = failwith "NYI: deps"

and simplify_effs _deps _k = failwith "NYI: effs"

and simplify_symbol sa sym error k =
  match Alpha.lookup sym sa with Error e -> error e | Ok id -> k id

and simplify_place (sa : Alpha.t) (place : Parse.place) (error : 'e -> expr)
    (k : Place.t -> expr) : expr =
  let rec inner_place pl k =
    match pl with
    | Parse.VarP sym ->
        simplify_symbol sa sym error @@ fun id -> k (Place.baseid id)
    | Parse.SlotP (pl, sl) ->
        inner_place pl @@ fun pl -> k (Place.slot (Slot.of_string sl) pl)
    | Parse.ArefP (pl, name) -> (
        inner_place pl
        @@ fun pl ->
        match int_of_string_opt name with
        | Some n ->
            k (Place.offset n pl)
        | None ->
            simplify_symbol sa name error @@ fun id -> k (Place.dynoffset id pl)
        )
  in
  inner_place place k

and simplify_expr (sa : Alpha.t) (expr : Parse.expr) (err : 'e -> expr)
    (k : Symbol.t -> (expr -> expr) -> expr) : expr =
  let basic_ectx expr =
    let bound = Symbol.fresh () in
    let ectx hole = LetE {bound; ty= Ty.unknown; expr; body= hole} in
    k bound ectx
  in
  match expr with
  | Parse.TrueE ->
      basic_ectx TrueE
  | Parse.FalseE ->
      basic_ectx FalseE
  | Parse.LitE n ->
      basic_ectx (LitE n)
  | Parse.StringE s ->
      basic_ectx (StringE s)
  | Parse.PlaceE pl ->
      simplify_place sa pl err @@ fun pl -> basic_ectx (PlaceE pl)
  | Parse.IfE (_test, _consequent, _alternate) ->
      failwith "NYI"
  | Parse.BeginE exprs ->
      let rec loop ectx = function
        | [] ->
            basic_ectx VoidE
        | [e] ->
            simplify_expr sa e err @@ fun id ectx' -> k id (ecompose ectx ectx')
        | e :: exprs ->
            simplify_expr sa e err
            @@ fun _ ectx' -> loop (ecompose ectx ectx') exprs
      in
      loop (fun x -> x) exprs
  | Parse.StructE fields ->
      let rec loop ectx acc = function
        | [] ->
            let bound = Symbol.fresh () in
            let ectx hole =
              LetE
                { bound
                ; ty= Ty.unknown
                ; expr= StructE (List.rev_map (fun (s, e) -> (s, e)) acc)
                ; body= hole }
            in
            k bound ectx
        | (slot, e) :: rest ->
            simplify_expr sa e err
            @@ fun id ectx' ->
            let slot = Slot.of_string slot in
            loop (ecompose ectx ectx') ((slot, id) :: acc) rest
      in
      loop (fun x -> x) [] fields
  | Parse.SetE (pl, e) ->
      simplify_place sa pl err
      @@ fun pl ->
      simplify_expr sa e err
      @@ fun id ectx ->
      let bound = Symbol.fresh () in
      let ectx hole =
        LetE {bound; ty= Ty.unknown; expr= SetE (pl, id); body= hole} |> ectx
      in
      k bound ectx
  | Parse.RefE e ->
      simplify_expr sa e err
      @@ fun id ectx ->
      let bound = Symbol.fresh () in
      let ectx hole =
        LetE {bound; ty= Ty.unknown; expr= RefE id; body= hole} |> ectx
      in
      k bound ectx
  | Parse.DerefE e ->
      simplify_expr sa e err
      @@ fun id ectx ->
      let bound = Symbol.fresh () in
      let ectx hole =
        LetE {bound; ty= Ty.unknown; expr= DerefE id; body= hole} |> ectx
      in
      k bound ectx
  | Parse.FnE (formals, body) ->
      let args, tys = List.split formals in
      simplify_func sa args tys body err
      @@ fun formals body ->
      let bound = Symbol.fresh () in
      let ectx hole =
        LetE {bound; ty= Ty.unknown; expr= FnE (formals, body); body= hole}
      in
      k bound ectx
  | Parse.AppE (s, ss) ->
      simplify_symbol sa s err
      @@ fun f ->
      simplify_exprs sa ss err
      @@ fun ss ectx ->
      let bound = Symbol.fresh () in
      let ectx hole =
        LetE
          { bound
          ; ty= Ty.unknown
          ; expr= AppE {f= Place.baseid f; args= ss}
          ; body= hole }
        |> ectx
      in
      k bound ectx

and simplify_exprs sa exprs err k =
  let rec loop ectx acc = function
    | [] ->
        k (List.rev acc) ectx
    | e :: rest ->
        simplify_expr sa e err
        @@ fun id ectx' -> loop (ecompose ectx ectx') (id :: acc) rest
  in
  loop (fun x -> x) [] exprs

and simplify_func sa pargs tys body err
    (k : (Symbol.t * Ty.t) list -> expr -> expr) =
  let args, sa = Alpha.extend' pargs sa in
  simplify_tys tys err
  @@ fun tys ->
  let formals = List.combine args tys in
  simplify_body sa body err @@ fun body -> k formals body

and simplify_def sa (name, expr) err (k : Alpha.t -> (expr -> expr) -> expr) =
  Logs.debug (fun m -> m "simplify_def %s" name) ;
  simplify_expr sa expr err
  @@ fun id ectx ->
  let bound, sa = Alpha.insert' name sa in
  let ectx hole =
    LetE {bound; ty= Ty.unknown; expr= PlaceE (Place.baseid id); body= hole}
    |> ectx
  in
  k sa ectx

and simplify_defn sa (name, formals, body) err
    (k : Alpha.t -> (expr -> expr) -> expr) =
  Logs.debug (fun m -> m "simplify_defn %s" name) ;
  let args, tys = List.split formals in
  let groupid, sa = Alpha.lookup_or_insert name sa in
  let overloadid = Symbol.derivative groupid in
  simplify_func sa args tys body err
  @@ fun formals fbody ->
  let ectx hole =
    LabelE {groupid; overloadid; args= formals; fbody; body= hole}
  in
  k sa ectx

and simplify_body sa (decls, expr) err (k : expr -> expr) : expr =
  let rec loop sa ectx k = function
    | [] ->
        k sa ectx
    | decl :: ds -> (
      match decl with
      | Parse.Def (name, expr) ->
          simplify_def sa (name, expr) err
          @@ fun sa ectx' -> loop sa (ecompose ectx ectx') k ds
      | Parse.Defn (name, formals, body) ->
          simplify_defn sa (name, formals, body) err
          @@ fun sa ectx' -> loop sa (ecompose ectx ectx') k ds )
  in
  loop sa
    (fun x -> x)
    (fun sa (ectx : expr -> expr) ->
      simplify_expr sa expr err
      @@ fun id ectx' ->
      let ectx = ecompose ectx ectx' in
      k (ectx (PlaceE (Place.baseid id))) )
    decls

let report_error (`Msg str) =
  Format.eprintf "Type error: %s@." str ;
  exit 1

let run (prog : Parse.program) : program =
  simplify_body Alpha.initial prog report_error @@ fun body -> body

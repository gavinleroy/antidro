[@@@warning "-27"]

open Ppx_hash_lib.Std
open Hash.Builtin
open Sexplib.Std
module Sexp = Sexplib.Sexp
open Util

type error = [`Msg of string] [@@deriving sexp_of, show]

module type Identifier = sig
  type t [@@deriving sexp_of, show, eq, ord, hash]

  val to_string : t -> string

  val compare : t -> t -> int

  val fresh : ?prefix:string -> unit -> t

  val var : string -> t

  val derivative : t -> t

  val raw : string -> t

  val pp : Format.formatter -> t -> unit
end

module Id : Identifier = struct
  type t = (string[@hash.ignore]) * int [@@deriving eq, ord, hash]

  let pp fmt (s, i) = Format.fprintf fmt "%s_%d" s i

  let to_string id = Format.asprintf "%a" pp id

  let show = to_string

  let sexp_of_t s = Sexp.Atom (to_string s)

  (* Remove all characters that can't appear in a JS symbol *)
  let sanitize s = String.map (function '-' -> '_' | '+' -> 'P' | c -> c) s

  let fresh =
    let counter = ref 0 in
    fun ?(prefix = "gensym") () ->
      incr counter ;
      (sanitize prefix, !counter)

  let var s = fresh ~prefix:s ()

  let derivative (s, _) = fresh ~prefix:s ()

  (* NOTE, zero is reserved for raw symbols. These are generated by the compiler *)
  (* and gauranteed to be unique in the defining context *)
  let raw s = (s, 0)

  let pp fmt s = to_string s |> Format.pp_print_text fmt
end

(* Represents symbols provided by the language *)
module Prim : Identifier = Id

module MetaVar : Identifier = Id

module TyVar : Identifier = Id

module Symbol : Identifier = Id

module Group : Identifier = Id

module SymbolMap = Map.Make (Symbol)

module Slot : sig
  type t [@@deriving sexp_of, show, eq, ord, hash]

  val return : t

  val broadcast : t

  val children : t

  val compare : t -> t -> int

  val to_string : t -> string

  val of_string : string -> t

  val pp : Format.formatter -> t -> unit
end = struct
  type t = string [@@deriving sexp_of, show, eq, ord, hash]

  let of_string s = s

  let to_string s = s

  let broadcast = of_string "broadcast"

  let return = of_string "ret"

  let children = of_string "children"

  let compare = String.compare

  let pp fmt s = Format.pp_print_string fmt s
end

module Place = struct
  type index = LitI of int | DynI of Symbol.t | SplatI
  [@@deriving sexp_of, eq, ord, hash]

  type adjustment = SlotAdj of Slot.t | OffAdj of index
  [@@deriving sexp_of, eq, ord, hash]

  type base = HoleB | ResultB | VarB of Symbol.t
  [@@deriving sexp_of, eq, ord, hash]

  (* NOTE, adjustment are stored in reverse order, *)
  (*  the place `posns[0].x` is represented as *)
  (*  ("posns, [Slot "x"; Offset 0]"). *)
  type t = base * adjustment list [@@deriving sexp_of, eq, ord, hash]

  let pp_base fmt = function
    | HoleB ->
        Format.pp_print_string fmt "<>"
    | ResultB ->
        Format.pp_print_string fmt "result"
    | VarB id ->
        Symbol.pp fmt id

  let pp_adj fmt = function
    | SlotAdj s ->
        Format.fprintf fmt ".%a" Slot.pp s
    | OffAdj (LitI n) ->
        Format.fprintf fmt "[%d]" n
    | OffAdj (DynI id) ->
        Format.fprintf fmt "[%a]" Symbol.pp id
    | OffAdj SplatI ->
        Format.fprintf fmt "[*]"

  let pp fmt (base, adjs) =
    Format.fprintf fmt "%a%a" pp_base base
      (Format.pp_list ~pp_sep:Format.pp_print_nothing pp_adj)
      (List.rev adjs)

  let subst bbs (base, adjs) =
    List.assoc_opt base bbs
    |> Option.map (fun id -> (VarB id, adjs))
    |> Option.value ~default:(base, adjs)

  let is_inner (_, adjs) = List.is_empty adjs |> not

  let hole = (HoleB, [])

  let result = (ResultB, [])

  let replace_inner ((b, adjs_inner) : t) ((_, adjs_outer) : t) : t =
    (b, adjs_inner @ adjs_outer)

  let is_hole (base, _) = base = HoleB

  let is_return (base, _) = base = ResultB

  let get_adjustments (_, adjs) = adjs

  let get_id_base = function VarB id, _ -> Some id | _, _ -> None

  let has_id_base id pl =
    get_id_base pl
    |> Option.map (Symbol.equal id)
    |> Option.value ~default:false

  let plug inner (pl : t) : t =
    if is_hole pl then replace_inner inner pl else pl

  let references id (base, _) =
    match base with VarB id' -> id = id' | _ -> false

  let result_to_hole ((base, adjs) : t) =
    match base with ResultB -> (HoleB, adjs) | _ -> (base, adjs)

  let to_result_base ((_, adjs) : t) : t = (ResultB, adjs)

  let swap_id ((f, pl') : Symbol.t * t) (pl : t) : t =
    if has_id_base f pl then replace_inner pl' pl else pl

  let base (b : base) : t = (b, [])

  let baseid (id : Symbol.t) : t = base (VarB id)

  let adj ((base, adjs) : t) (adj : adjustment) = (base, adj :: adjs)

  let offset (n : int) (pl : t) : t = adj pl (OffAdj (LitI n))

  let offsetid (n : int) (id : Symbol.t) : t = offset n (baseid id)

  let dynoffset (id : Symbol.t) (pl : t) : t = adj pl (OffAdj (DynI id))

  let slot (sl : Slot.t) (pl : t) : t = adj pl (SlotAdj sl)

  let slotid (sl : Slot.t) (id : Symbol.t) : t = slot sl (baseid id)

  let to_string (base, adjs) =
    let base_str =
      match base with
      | HoleB ->
          "hole"
      | ResultB ->
          "result"
      | VarB id ->
          Symbol.to_string id
    in
    let index_str = function
      | LitI n ->
          string_of_int n
      | DynI id ->
          Symbol.to_string id
      | SplatI ->
          "splat"
    in
    let adjs_str =
      List.rev_map
        (function
          | SlotAdj sl -> Slot.to_string sl | OffAdj i -> "off_" ^ index_str i
          )
        adjs
    in
    String.concat "_" ("updater" :: base_str :: adjs_str)

  let to_updater_id (pl : t) : Symbol.t = to_string pl |> Symbol.raw

  let to_updater_slot (pl : t) : Slot.t =
    to_string (to_result_base pl) |> Slot.of_string
end

module type PlaceToPlaces = sig
  type t [@@deriving sexp_of, show, eq, ord]

  val destructure : t -> Place.t * Place.t list

  val on : Place.t -> Place.t list -> t
end

module Dep = struct
  type t = Place.t * Place.t list [@@deriving eq, ord]

  let pp fmt (pl, pls) =
    Format.fprintf fmt "%a^{%a}" Place.pp pl (Format.pp_list Place.pp) pls

  let show = Format.asprintf "%a" pp

  let sexp_of_t t = Sexp.Atom (Format.asprintf "%a" pp t)

  let destructure t = t

  let deps pls = (Place.hole, pls)

  let on (pl : Place.t) (pls : Place.t list) : t = (pl, pls)

  let is_result_dep (pl, _) = Place.get_id_base pl |> Option.is_none

  let is_inner_dep (pl, _) = Place.is_inner pl
end

(* NOTE, all effects are `Write` effects, so no distinction here *)
module Eff = struct
  type t = Place.t * Place.t list [@@deriving eq, ord]

  let pp fmt (pl, pls) =
    Format.fprintf fmt "Write<{%a}, %a>" (Format.pp_list Place.pp) pls Place.pp
      pl

  let sexp_of_t t = Sexp.Atom (Format.asprintf "%a" pp t)

  let show = Format.asprintf "%a" pp

  let destructure t = t

  let on (pl : Place.t) (pls : Place.t list) : t = (pl, pls)
end

module PlaceMap = struct
  module M = Map.Make (Place)
  module S = Set.Make (Place)

  type t = S.t M.t

  module Impl (Elt : PlaceToPlaces) : sig
    type elt = Elt.t

    val union_to : Place.t -> S.t -> t -> t

    val add_to : Place.t -> Place.t list -> t -> t

    val empty : t

    val of_list : elt list -> t

    val to_list : t -> elt list

    val singleton : elt -> t

    val on_opt : Place.t -> t -> Place.t list option

    val on : Place.t -> t -> Place.t list

    val adjust : (Place.t -> Place.t) -> t -> t

    val sexp_of_t : t -> Sexp.t

    val pp : Format.formatter -> t -> unit

    val merge : t -> t -> t

    val ( @ ) : t -> t -> t

    val invert : t -> t

    val assert_empty : t -> (unit, error) result

    val equal : t -> t -> bool
  end = struct
    type elt = Elt.t

    let union_to (k : S.elt) (s : S.t) (m : t) : t =
      M.find_opt k m
      |> Option.value ~default:S.empty
      |> S.union s
      |> fun s -> M.add k s m

    let add_to (k : S.elt) (ls : Place.t list) (m : t) : t =
      union_to k (S.of_list ls) m

    let empty = M.empty

    let of_list (ls : Elt.t list) : t =
      List.fold_left
        (fun m dep ->
          let pl, pls = Elt.destructure dep in
          add_to pl pls m )
        M.empty ls

    let to_list (m : t) : Elt.t list =
      M.to_list m |> List.map (fun (pl, pls) -> Elt.on pl (S.to_list pls))

    let singleton (dep : Elt.t) : t = of_list [dep]

    let on_opt (pl : Place.t) (m : t) : Place.t list option =
      M.find_opt pl m |> Option.map S.elements

    let on (pl : Place.t) (m : t) : Place.t list =
      on_opt pl m |> Option.value ~default:[]

    let sexp_of_t m = Sexp.List (to_list m |> List.map Elt.sexp_of_t)

    let pp fmt m = Format.fprintf fmt "%a" (Format.pp_list Elt.pp) (to_list m)

    let merge m1 m2 = M.fold (fun k v m -> union_to k v m) m2 m1

    let ( @ ) = merge

    let invert (m : t) : t =
      M.fold
        (fun k v m -> List.fold_right (fun v -> add_to v [k]) (S.elements v) m)
        m M.empty

    let adjust (f : Place.t -> Place.t) (m : t) : t =
      M.fold (fun k v -> M.add (f k) (S.map f v)) m M.empty

    let assert_empty (m : t) : (unit, error) result =
      if M.is_empty m then Result.ok ()
      else
        Result.error
          (`Msg (Format.asprintf "expected empty set, but got: %a" pp m))

    let equal (m1 : t) (m2 : t) : bool = M.equal S.equal m1 m2
  end
end

module Dependencies = struct
  include PlaceMap.Impl (Dep)

  type t = PlaceMap.t

  let adjust_lhs f m =
    PlaceMap.M.fold (fun k v m -> PlaceMap.M.add (f k) v m) m PlaceMap.M.empty

  (* Replace dependencies in `t` that reference a place built on `id`, using the *)
  (* dependencies in `idbound`. *)
  let purge (id : Symbol.t) (idbound : t) (t : t) : t =
    let purge_set (s : PlaceMap.S.t) =
      PlaceMap.S.fold
        (fun el s ->
          if Place.references id el then
            PlaceMap.M.find_opt el idbound
            |> Option.value ~default:PlaceMap.S.empty
            |> PlaceMap.S.union s
          else PlaceMap.S.add el s )
        s PlaceMap.S.empty
    in
    PlaceMap.M.map purge_set t
end

module Effects = struct
  include PlaceMap.Impl (Eff)

  type t = PlaceMap.t

  let extend (d : Dependencies.t) (m : t) =
    List.fold_left
      (fun m dep ->
        let pl, pls = Dep.destructure dep in
        add_to pl pls m )
      m (Dependencies.to_list d)

  let ( ++ ) m d = extend d m

  let remove (id : Symbol.t) (m : t) : t =
    PlaceMap.M.filter (fun pl _ -> not (Place.references id pl)) m
end

type ty =
  | MetaT of MetaVar.t
  | VarT of TyVar.t
  | DepsT of Dependencies.t
  | EffsT of Effects.t
  | GroupT of Group.t
  | AppT of tycon * ty list
  | PolyT of TyVar.t list * ty
[@@deriving sexp_of, show]

and tycon =
  | VoidC
  | NumberC
  | StringC
  | BoolC
  (* NOTE: The arguments t1, t2, ..., tn *)
  (* to an ArrowC asr *)
  (* t1: dependency set *)
  (* t2: effect set *)
  (* t3: return value *)
  (* t4-tn: arguments *)
  | ArrowC of Symbol.t list
  | ArrayC
  | RefC
  | StructC of Slot.t list
  | TyFnC of TyVar.t list * ty
[@@deriving sexp_of, show]

module SymbMap = Map.Make (Symbol)
module TyMap = Map.Make (TyVar)

type idsubst = Symbol.t SymbMap.t

type ssubst = ty SymbMap.t

type tysubst = ty TyMap.t

module TyCon = struct
  type t = tycon [@@deriving sexp_of, show]
end

module Ty = struct
  module TMap = Map.Make (TyVar)

  type t = ty [@@deriving sexp_of, show]

  let new_meta () : t = MetaT (MetaVar.fresh ())

  let new_method_group () : t = GroupT (Group.fresh ())

  module Meta = struct
    module MTable = Hashtbl.Make (MetaVar)

    let sm : ty MTable.t ref = ref (MTable.create 1000)

    let insert v = MTable.add !sm v

    let lookup v = MTable.find_opt !sm v

    let find v = MTable.find !sm v

    let mem v = MTable.mem !sm v

    let transaction f =
      let saved = MTable.copy !sm in
      let res = f () in
      sm := saved ;
      res
  end

  let unknown = new_meta ()

  let bool = AppT (BoolC, [])

  let number = AppT (NumberC, [])

  let string = AppT (StringC, [])

  let void = AppT (VoidC, [])

  let ref (t : t) : t = AppT (RefC, [t])

  let ref_inner_ty = function AppT (RefC, [t]) -> Some t | _ -> None

  let array (t : t) : t = AppT (ArrayC, [t])

  let struct_ (fields : (Slot.t * t) list) : t =
    let fields, tys = List.split fields in
    AppT (StructC fields, tys)

  let func ?(deps : t = new_meta ()) ?(effs : t = new_meta ())
      (formals : (Symbol.t * ty) list) (ret : ty) : ty =
    let names, tys = List.split formals in
    let tys = deps :: effs :: ret :: tys in
    AppT (ArrowC names, tys)

  let arrow ?(tyvars : TyVar.t list = []) ?(deps : t = new_meta ())
      ?(effs : t = new_meta ()) (formals : (Symbol.t * ty) list) (ret : ty) : ty
      =
    PolyT (tyvars, func ~deps ~effs formals ret)

  let dependencies (ls : Dependencies.t) = DepsT ls

  let effects (ls : Effects.t) = EffsT ls

  let is_ref = function AppT (RefC, _) -> true | _ -> false

  let as_group (t : t) : (Group.t, error) result =
    match t with
    | GroupT id ->
        Result.ok id
    | _ ->
        Result.error (`Msg "expected a group")

  let is_group (t : t) : bool = as_group t |> Result.is_ok

  let as_deps (t : t) : (Dependencies.t, error) result =
    match t with
    | DepsT d ->
        Result.ok d
    | _ ->
        Result.error
          (`Msg (Format.asprintf "expected a dependencies set but got: %a" pp t))

  let as_effs (t : t) : (Effects.t, error) result =
    match t with
    | EffsT e ->
        Result.ok e
    | _ ->
        Result.error
          (`Msg (Format.asprintf "expected an effects set but got: %a" pp t))

  let struct_fields (t : t) : ((Slot.t * t) list, error) result =
    match t with
    | AppT (StructC fields, tys) ->
        List.zip fields tys
    | _ ->
        Result.error
          (`Msg (Format.asprintf "expected a struct type, but got: %a" pp t))

  let lookup_slot (s : Slot.t) (t : t) : (t, error) result =
    let open ResultMonad in
    let* zipped = struct_fields t in
    match List.find_opt (fun (f, _) -> Slot.equal f s) zipped with
    | Some (_, ty) ->
        return ty
    | None ->
        error
          (`Msg (Format.asprintf "slot %a not found in type %a" Slot.pp s pp t))

  let subst_of tvars tys : tysubst = List.combine tvars tys |> TMap.of_list

  let subst_of' ids ids' : idsubst = List.combine ids ids' |> SymbMap.of_list

  let rec subst (s : tysubst) (t : t) =
    match t with
    | VarT id ->
        TMap.find_opt id s |> Option.value ~default:t
    | MetaT a -> (
        Meta.lookup a |> function Some t -> subst s t | None -> t )
    | AppT (TyFnC (tvars, ty), tys) ->
        subst_of tvars tys |> fun s' -> subst s ty |> subst s
    | AppT (tycon, tys) ->
        AppT (tycon, List.map (subst s) tys)
    | PolyT (tvars, ty) ->
        let fresh = List.map (fun _ -> TyVar.fresh ()) tvars in
        let tys = List.map (fun id -> VarT id) fresh in
        let s' = subst_of tvars tys in
        let ty' = subst s' ty in
        PolyT (fresh, subst s ty')
    | t ->
        t

  let rec subst' (s : idsubst) (t : t) =
    match t with
    | PolyT (tvars, ty) ->
        PolyT (tvars, subst' s ty)
    | AppT (ArrowC ids, args) ->
        let fresh = List.map Symbol.derivative ids in
        let s' = subst_of' ids fresh in
        let args = List.map (subst' s') args |> List.map (subst' s) in
        AppT (ArrowC ids, args)
    | AppT (tycon, tys) ->
        AppT (tycon, List.map (subst' s) tys)
    | DepsT deps ->
        Dependencies.adjust
          (fun pl ->
            match Place.get_id_base pl with
            | None ->
                pl
            | Some id ->
                let id' = SymbMap.find_opt id s |> Option.value ~default:id in
                Place.swap_id (id, Place.baseid id') pl )
          deps
        |> fun t -> DepsT t
    | EffsT effs ->
        Effects.adjust
          (fun pl ->
            match Place.get_id_base pl with
            | None ->
                pl
            | Some id ->
                let id' = SymbMap.find_opt id s |> Option.value ~default:id in
                Place.swap_id (id, Place.baseid id') pl )
          effs
        |> fun t -> EffsT t
    | t ->
        t

  let unify_error t u =
    Result.error
      (`Msg
        (Format.asprintf "unification failed between:@\n%a@\nand@\n%a" pp t pp u)
        )

  let rec unify' (t : t) (u : t) : (unit, error) result =
    let error () = unify_error t u in
    match (t, u) with
    | VarT a, VarT b when a = b ->
        Result.ok ()
    | MetaT a, t -> (
        Meta.lookup a
        |> function
        | Some t' ->
            unify' t' t
        | None -> (
          match t with
          | AppT (TyFnC _, _) ->
              unify' (MetaT a) (expand u)
          | MetaT b when Meta.mem b ->
              unify' (MetaT a) (Meta.find b)
          | MetaT b when a = b ->
              Result.ok ()
          | _ when not (occurs (MetaT a) t) ->
              Meta.insert a t |> Result.ok
          | _ ->
              error () ) )
    | t, MetaT a ->
        unify' (MetaT a) t
    | AppT (ArrowC ids, args), AppT (ArrowC ids', args')
      when List.length ids = List.length ids' ->
        (* substitute ids' for ids within args', then unify' everything *)
        let s = subst_of' ids' ids in
        let args' = List.map (subst' s) args' in
        List.fold_left2
          (fun acc ty1 ty2 -> Result.bind acc (fun () -> unify' ty1 ty2))
          (Result.ok ()) args args'
    | AppT (tycon1, tys1), AppT (tycon2, tys2) when tycon1 = tycon2 ->
        List.fold_left2
          (fun acc ty1 ty2 -> Result.bind acc (fun () -> unify' ty1 ty2))
          (Result.ok ()) tys1 tys2
    | AppT (TyFnC (tyvars, u), tys), t ->
        subst (subst_of tyvars tys) u |> fun u' -> unify' u' t
    | t, AppT (TyFnC (tyvars, u), tys) ->
        subst (subst_of tyvars tys) u |> unify' t
    | PolyT (tyvars, u), PolyT (tyvars', u') ->
        let s' = subst_of tyvars' (List.map (fun id -> VarT id) tyvars) in
        subst s' u' |> unify' u
    | DepsT d1, DepsT d2 when Dependencies.equal d1 d2 ->
        Result.ok ()
    | EffsT e1, EffsT e2 when Effects.equal e1 e2 ->
        Result.ok ()
    | _ ->
        error ()

  and unify (t : t) (u : t) : (unit, error) result =
    try unify' t u with Invalid_argument _ -> unify_error t u

  and expand (t : t) : t =
    match t with
    | AppT (TyFnC (tvars, u), tys) ->
        subst (subst_of tvars tys) u |> expand
    | MetaT a when Meta.mem a ->
        expand (Meta.find a)
    | t ->
        t

  and occurs (u : t) (t : t) : bool =
    match t with
    | MetaT a ->
        Meta.lookup a
        |> Option.map (fun t' -> occurs u t')
        |> Option.value ~default:false
    | AppT (_, tys) ->
        List.exists (occurs u) tys
    | PolyT (_, k) ->
        occurs u k
    | q when q = u ->
        true
    | _ ->
        false

  let rec meta_vars_in (t : t) : MetaVar.t list =
    match t with
    | MetaT m ->
        [m]
    | AppT (_, ts) ->
        List.concat_map meta_vars_in ts
    | PolyT (_, t) ->
        meta_vars_in t
    | _ ->
        []

  let generalize (s : ssubst) (t : t) : t =
    let t = subst TyMap.empty t in
    Logs.debug (fun m -> m "generalizing %a" pp t) ;
    let metas =
      meta_vars_in t
      |> List.filter (fun v ->
             not (SymbMap.exists (fun _ t' -> occurs (MetaT v) t') s) )
    in
    let tyvars = List.map (fun _ -> TyVar.fresh ()) metas in
    List.iter2 (fun m tv -> Meta.insert m (VarT tv)) metas tyvars ;
    let t = PolyT (tyvars, t) in
    Logs.debug (fun m -> m "generalized to: %a" pp t) ;
    t

  let instantiate (t : t) : t =
    match t with
    | PolyT (tvars, t) ->
        let mvars = List.map (fun _ -> new_meta ()) tvars in
        subst (subst_of tvars mvars) t
    | _ ->
        t
end

module Signature = struct
  open Util.ResultMonad

  type t =
    { tyvars: TyVar.t list
    ; ids: Symbol.t list
    ; args: ty list
    ; deps: ty
    ; effs: ty
    ; return: ty }
  [@@deriving sexp_of, show]

  let formals s = List.combine s.ids s.args

  let from_ty : ty -> (t, 'e) result = function
    | PolyT (tyvars, AppT (ArrowC ids, deps :: effs :: return :: args)) ->
        if not (List.is_empty tyvars) then
          Logs.err (fun m ->
              m "tyvars are not empty %a" (Format.pp_list TyVar.pp) tyvars ) ;
        Result.ok {tyvars; ids; args; deps; effs; return}
    | t ->
        Result.error
          (`Msg
            ( "expected a polymorphic function, but got: "
            ^ Sexp.to_string_hum (sexp_of_ty t) ) )

  let from_instantiated : ty -> (t, 'e) result = function
    | AppT (ArrowC ids, deps :: effs :: return :: args) ->
        Result.ok {tyvars= []; ids; args; deps; effs; return}
    | t ->
        Result.error
          (`Msg
            ( "expected an instantiated function, but got: "
            ^ Sexp.to_string_hum (sexp_of_ty t) ) )

  let to_ty ({tyvars; ids; args; deps; effs; return} : t) : ty =
    PolyT (tyvars, AppT (ArrowC ids, deps :: effs :: return :: args))

  let is t = from_ty t |> Result.is_ok

  let uninstantiated_deps self : Dependencies.t option =
    match self.deps with DepsT deps -> Some deps | _ -> None

  let return (ty : ty) : (ty, 'e) result =
    let+ {return; _} = from_instantiated ty in
    return

  let args (ty : ty) : ((Symbol.t * ty) list, 'e) result =
    let* {ids; args; _} = from_instantiated ty in
    List.zip ids args

  let dependencies (args : Place.t list) (ty : ty) : (Dependencies.t, 'e) result
      =
    let* {ids; deps; _} = from_instantiated ty in
    let* deps = Ty.as_deps deps in
    let+ map = List.zip ids args in
    let deps_after =
      List.fold_right
        (fun swap -> Dependencies.adjust (Place.swap_id swap))
        map deps
      |> Dependencies.adjust_lhs Place.result_to_hole
    in
    Logs.debug (fun m ->
        m "instantiating-dependencies@\nmap: %a@\nbefore:@\n%a@\nafter:@\n%a"
          (Format.pp_list (fun fmt (id, pl) ->
               Format.fprintf fmt "%a: %a" Symbol.pp id Place.pp pl ) )
          map Dependencies.pp deps Dependencies.pp deps_after ) ;
    deps_after

  let effects (args : Place.t list) (ty : ty) : (Effects.t, 'e) result =
    let* {ids; effs; _} = from_instantiated ty in
    let* effs = Ty.as_effs effs in
    Logs.debug (fun m ->
        m "instantiating-effects with (effs: %a) (args: %a)" Effects.pp effs
          (Format.pp_list Place.pp) args ) ;
    let+ map = List.zip ids args in
    let instantiate_effect eff =
      let pl, pls = Eff.destructure eff in
      (* For each affecting place, we swap it with its dependencies  *)
      let pls =
        List.fold_right
          (fun swap pls -> List.map (Place.swap_id swap) pls)
          map pls
      in
      let pl = List.fold_right Place.swap_id map pl in
      Eff.on pl pls
    in
    List.map instantiate_effect (Effects.to_list effs) |> Effects.of_list

  (* Resolve the specific method overload from the MethodGroup in `fty` *)
  let resolve_application (overloads : (Symbol.t * Ty.t) list)
      (argstys : Ty.t list) : (Symbol.t * Ty.t, error) result =
    let open Util.ResultMonad in
    Logs.debug (fun m ->
        m "Resolving application of args@\n@[%a@]@\nwith overloads@\n@[%a@]\n"
          (Format.pp_list Ty.pp) argstys
          ( Format.pp_list ~pp_sep:Format.pp_print_newline
          @@ fun fmt (id, fty) ->
          Format.fprintf fmt "%a: %a" Symbol.pp id Ty.pp fty )
          overloads ) ;
    let unify_overload (_, fty) =
      let fty = Ty.instantiate fty in
      let* ids = args fty |> Result.map (List.map fst) in
      let mret = Ty.new_meta () in
      let* args = List.zip ids argstys in
      let msig = Ty.func args mret in
      Logs.debug (fun m ->
          m "checking overload with signature@\n@[%a@]@\n@[%a@]" Ty.pp fty Ty.pp
            msig ) ;
      Ty.unify fty msig
    in
    (* Run the checking in a transaction so that we don't clutter the meta variables *)
    let is_applicable_overload overload =
      Ty.Meta.transaction (fun () -> unify_overload overload |> Result.is_ok)
    in
    match List.filter is_applicable_overload overloads with
    | [] ->
        error (`Msg "no matching overload")
    | [(id, fty)] ->
        return (id, fty |> Ty.instantiate)
    | _ ->
        error (`Msg "Ambiguous application, multiple matching overloads")
end

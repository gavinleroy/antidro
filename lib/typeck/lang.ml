open Sexplib.Std
module Sexp = Sexplib.Sexp

module Slot : sig
  type t [@@deriving sexp, show]

  val box_value : t

  val return : t

  val compare : t -> t -> int

  val to_string : t -> string

  val of_string : string -> t

  val pp : Format.formatter -> t -> unit
end = struct
  type t = string [@@deriving sexp, show]

  let of_string s = s

  let to_string s = s

  let box_value = of_string "value"

  let return = of_string "retval"

  let compare = String.compare

  let pp fmt s = Format.pp_print_string fmt s
end

module Identifier : sig
  type t [@@deriving sexp, show]

  val to_string : t -> string

  val compare : t -> t -> int

  val fresh : ?capital:bool -> ?prefix:string -> unit -> t

  val var : ?capital:bool -> string -> t

  val raw : string -> t

  val pp : Format.formatter -> t -> unit
end = struct
  type t = string [@@deriving sexp, show]

  let to_string s = s

  let compare = String.compare

  let sanitize s = String.map (function '-' -> '_' | '+' -> 'P' | c -> c) s

  let fresh =
    let counter = ref 0 in
    fun ?(capital = false) ?(prefix = "gensym") () ->
      let prefix = if capital then String.capitalize_ascii prefix else prefix in
      incr counter ;
      Printf.sprintf "t_%s_%d" (sanitize prefix) !counter

  let var ?(capital = false) s = fresh ~capital ~prefix:s ()

  let raw s = s

  let pp fmt s = Format.pp_print_string fmt s
end

module IdMap = Map.Make (Identifier)

module Place = struct
  type adjustment =
    | SplatAdj
    | SlotAdj of Slot.t
    | OffAdj of int
    | DynOffAdj of Identifier.t
  [@@deriving sexp, show]

  type base = ResultB | VarB of Identifier.t [@@deriving sexp, show]

  (* NOTE, adjustment are stored in reverse order, *)
  (*  the place `posns[0].x` is represented as *)
  (*  ("posns, [Slot "x"; Offset 0]"). *)
  type t = base * adjustment list [@@deriving sexp, show]

  let compare (base1, adjs1) (base2, adjs2) =
    let compare_base () =
      match (base1, base2) with
      | ResultB, ResultB ->
          0
      | ResultB, _ ->
          -1
      | _, ResultB ->
          1
      | VarB id1, VarB id2 ->
          Identifier.compare id1 id2
    and compare_adjs () =
      let rec loop adjs1 adjs2 =
        match (adjs1, adjs2) with
        | [], [] ->
            0
        | [], _ ->
            -1
        | _, [] ->
            1
        | adj1 :: adjs1, adj2 :: adjs2 when adj1 = adj2 ->
            loop adjs1 adjs2
        | SlotAdj sl1 :: _, SlotAdj sl2 :: _ ->
            Slot.compare sl1 sl2
        | OffAdj n1 :: _, OffAdj n2 :: _ ->
            compare n1 n2
        | DynOffAdj n1 :: _, DynOffAdj n2 :: _ ->
            compare n1 n2
        | SplatAdj :: _, _ ->
            -1
        | _, SplatAdj :: _ ->
            1
        | _, _ ->
            -1
      in
      let l1 = List.length adjs1 and l2 = List.length adjs2 in
      if l1 = l2 then loop adjs1 adjs2 else compare l1 l2
    in
    match compare_base () with 0 -> compare_adjs () | n -> n

  let subst bbs (base, adjs) =
    List.assoc_opt base bbs
    |> Option.map (fun id -> (VarB id, adjs))
    |> Option.value ~default:(base, adjs)

  let is_inner (_, adjs) = List.is_empty adjs |> not

  let func_result = (ResultB, [])

  let replace_fres (base, adjs) id =
    match base with ResultB -> (VarB id, adjs) | _ -> (base, adjs)

  let get_id_base = function ResultB, _ -> None | VarB id, _ -> Some id

  let base (b : base) : t = (b, [])

  let baseid id : t = base (VarB id)

  let adj ((base, adjs) : t) (adj : adjustment) = (base, adj :: adjs)

  let deref pl = adj pl (SlotAdj Slot.box_value)

  let derefid id = deref (baseid id)

  let offset (n : int) (pl : t) : t = adj pl (OffAdj n)

  let offsetid (n : int) (id : Identifier.t) : t = offset n (baseid id)

  let dynoffset (id : Identifier.t) (pl : t) : t = adj pl (DynOffAdj id)

  let slot (sl : Slot.t) (pl : t) : t = adj pl (SlotAdj sl)

  let slotid (sl : Slot.t) (id : Identifier.t) : t = slot sl (baseid id)

  let to_string (base, adjs) =
    let base_str =
      match base with ResultB -> "result" | VarB id -> Identifier.to_string id
    in
    let adjs_str =
      List.map
        (function
          | SplatAdj ->
              "splat"
          | SlotAdj sl ->
              Slot.to_string sl
          | OffAdj n ->
              String.concat "" ["off_"; string_of_int n]
          | DynOffAdj id ->
              String.concat "" ["dynoff_"; Identifier.to_string id] )
        adjs
    in
    String.concat "_" ("updater" :: base_str :: adjs_str)

  let to_updater_id (pl : t) : Identifier.t = to_string pl |> Identifier.raw

  let to_updater_slot (pl : t) : Slot.t = to_string pl |> Slot.of_string
end

module IdSet = Set.Make (Identifier)

type dep = DepsOn of Place.t * Place.t list [@@deriving sexp, show]

and ty_binding = Identifier.t * ty [@@deriving sexp, show]

and ty_bindings = ty_binding list [@@deriving sexp, show]

and dependencies = Opaque of Identifier.t | Transparent of dep list
[@@deriving sexp, show]

and effects = Opaque of Identifier.t | Transparent of eff list
[@@deriving sexp, show]

and eff = WriteEf of Place.t [@@deriving sexp, show]

and 'a binder =
  { types: Identifier.t list
  ; depset: Identifier.t option
  ; effset: Identifier.t option
  ; value: signature }

and signature =
  { args: ty_bindings
  ; return: ty
  ; dependencies: dependencies
  ; effects: effects
  ; freevars: Identifier.t list }
[@@deriving sexp, show]

and polysig = signature binder [@@deriving sexp, show]

and methd = {id: Identifier.t; group: Identifier.t; signature: polysig}
[@@deriving sexp, show]

and ty =
  | VoidT
  | BoolT
  | NumberT
  | StringT
  | UncallableT
  | VarT of Identifier.t
  | ArrayT of ty
  | StructT of (Slot.t * ty) list
  | FnT of polysig
  | MethodGroupT of Identifier.t
[@@deriving sexp, show]

and expr =
  | VoidE
  | TrueE
  | FalseE
  | LitE of int
  | StringE of string
  | PlaceE of Place.t
  | LetE of Identifier.t * ty * expr * expr
  | StructE of (Slot.t * Identifier.t) list
  | NewE of Identifier.t * Identifier.t list
  | FnE of polysig * expr
  | AppE of Identifier.t * Identifier.t list
  | SetE of Place.t * Identifier.t
[@@deriving sexp, show]

module Signature = struct
  type t = polysig

  let to_string s = Sexp.to_string_hum (sexp_of_polysig s)

  let basic ?(deps = []) ?(effs = []) ?(freevars = []) args return : t =
    { types= []
    ; depset= None
    ; effset= None
    ; value=
        { args
        ; return
        ; dependencies= Transparent deps
        ; effects= Transparent effs
        ; freevars } }

  let basic' ?(deps = []) ?(effs = []) ?(freevars = []) args return : t =
    let args = List.map (fun (s, ty) -> (Identifier.raw s, ty)) args in
    basic ~deps ~effs ~freevars args return

  let is_empty s =
    List.is_empty s.types && Option.is_none s.depset && Option.is_none s.effset

  let polyall ?(freevars = []) args return : t =
    let eff = Identifier.fresh ~prefix:"eff" () in
    let dep = Identifier.fresh ~prefix:"dep" () in
    { types= []
    ; depset= None
    ; effset= Some eff
    ; value=
        {args; return; dependencies= Opaque dep; effects= Opaque eff; freevars}
    }

  let skip_binders s : signature = s.value

  let args (ps : t) : Identifier.t list = List.map fst ps.value.args

  let return s = s.value.return

  let effects (ps : t) =
    match ps.value.effects with
    | Opaque _ ->
        failwith "cannot query effectiveness of opaque set"
    | Transparent effs ->
        effs

  let is_effectful (ps : t) = effects ps |> List.is_empty |> not

  let freevars (ps : t) : Identifier.t list = ps.value.freevars

  (* The list of identifiers captured from the surrounding environment. *)
  (* This includes free variables, as well as updating functions for written places. *)
  let upvars (ps : t) : Identifier.t list =
    let freevars = freevars ps in
    let effvars =
      effects ps |> List.map (function WriteEf pl -> Place.to_updater_id pl)
    in
    freevars @ effvars
end

module Ty = struct
  type t = ty [@@deriving sexp, show]

  let to_string s = Sexp.to_string_hum (sexp_of_ty s)

  let is_callable = function FnT _ -> true | _ -> false

  let as_ref ty = StructT [(Slot.box_value, ty)]

  let ref_ty = function
    | StructT [(sl, ty)] when sl = Slot.box_value ->
        Some ty
    | _ ->
        None

  let is_ref ty = ref_ty ty |> Option.is_some

  let slot_type ty sl =
    match ty with
    | StructT fields ->
        List.assoc_opt sl fields
    | ty -> (
      match ref_ty ty with
      | Some (StructT fields) ->
          List.assoc_opt sl fields |> Option.map as_ref
      | _ ->
          None )

  let element_ty ty =
    match ty with
    | ArrayT ty ->
        Some ty
    | ty -> (
      match ref_ty ty with Some (ArrayT ty) -> Some (as_ref ty) | _ -> None )

  let updater_arg = Identifier.var "to_update"

  let updater_ty ty = FnT (Signature.basic [(updater_arg, ty)] VoidT)
end

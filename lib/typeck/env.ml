open Ant.Ty
open Ant.Util
open Lang

module Alpha = struct
  module M = Map.Make (String)

  type t = Symbol.t M.t

  let empty : t = M.empty

  let lookup (key : M.key) (self : t) : (Symbol.t, 'a) result =
    M.find_opt key self
    |> function
    | Some a -> Ok a | None -> Error (`Msg ("unbound variable: " ^ key))

  let insert (key : M.key) (sym : Symbol.t) (self : t) : t = M.add key sym self

  let insert' (key : M.key) (self : t) : Symbol.t * t =
    let sym = Symbol.var key in
    (sym, insert key sym self)

  let lookup_or_insert (key : string) (self : t) : Symbol.t * t =
    match M.find_opt key self with
    | Some a ->
        (a, self)
    | None ->
        insert' key self

  let extend (ss : (M.key * Symbol.t) list) (self : t) : t =
    List.fold_left (fun acc (k, v) -> insert k v acc) self ss

  let extend' (ss : M.key list) (self : t) : Symbol.t list * t =
    List.fold_left
      (fun (acc, env) k ->
        let sym, env = insert' k env in
        (sym :: acc, env) )
      ([], self) ss

  let initial =
    List.fold_left
      (fun self -> function
        | AntStd.MethodPrim {name; groupid= id; _}
        | AntStd.FuncPrim {name; id; _} ->
            insert name id self )
      empty AntStd.pervasives
end

module Gamma = struct
  module M = SymbolMap
  module G = Map.Make (Group)

  type t = {vars: Ty.t M.t; overloads: Symbol.t list G.t}

  type error = [`Msg of string]

  let empty =
    let vars = M.empty and overloads = G.empty in
    {vars; overloads}

  let ty_subst (self : t) : ssubst = self.vars

  let lookup (key : M.key) (self : t) : (Ty.t, error) result =
    match M.find_opt key self.vars with
    | Some ty ->
        Ok ty
    | None ->
        Error (`Msg ("unbound variable: " ^ Symbol.show key))

  let insert (key : M.key) (ty : Ty.t) (self : t) : t =
    {self with vars= M.add key ty self.vars}

  let extend (ls : (M.key * Ty.t) list) (self : t) : t =
    List.fold_left (fun acc (sym, ty) -> insert sym ty acc) self ls

  let add_method (name : Symbol.t)
      ?(overloadid : Symbol.t = Symbol.derivative name) (ty : Ty.t) (self : t) :
      (Symbol.t * t, error) result =
    let open ResultMonad in
    let gty =
      lookup name self
      |> function Ok ty -> ty | Error _ -> Ty.new_method_group ()
    in
    let+ group = Ty.as_group gty in
    if not (Signature.is ty) then (
      Logs.err (fun m -> m "expected method signature %a" Ty.pp ty) ;
      assert false ) ;
    let vars = M.add name gty self.vars |> M.add overloadid ty in
    let self = {self with vars} in
    let overloads =
      G.find_opt group self.overloads |> Option.value ~default:[]
    in
    ( overloadid
    , {self with overloads= G.add group (overloadid :: overloads) self.overloads}
    )

  let methods (group : Group.t) (self : t) : (Symbol.t * Ty.t) list =
    G.find_opt group self.overloads
    |> Option.value ~default:[]
    |> List.map (fun s -> (s, M.find s self.vars))

  let initial =
    List.fold_left
      (fun (self : t) -> function
        | AntStd.MethodPrim {groupid; overloads; _} ->
            List.fold_left
              (fun self ({id= overloadid; ty; _} : AntStd.overload_data) ->
                add_method ~overloadid groupid ty self |> Result.unwrap |> snd
                )
              self overloads
        | AntStd.FuncPrim {id; ty; _} ->
            insert id ty self )
      empty AntStd.pervasives

  let is_primitive (pl : Place.t) : bool =
    Place.get_id_base pl
    |> Option.map (fun id -> M.find_opt id initial.vars |> Option.is_some)
    |> Option.value ~default:false

  let as_single_overload (t : Ty.t) (self : t) : (Symbol.t * Ty.t, 'e) result =
    let open ResultMonad in
    let* gid = Ty.as_group t in
    match methods gid self with
    | [(id, ty)] ->
        return (id, ty)
    | _ ->
        error (`Msg "expected single overload")
end

module Theta = struct
  module M = TyMap

  type t = TyCon.t M.t

  let empty = M.empty

  let insert (key : M.key) (ty : TyCon.t) (self : t) : t = M.add key ty self

  let initial =
    List.fold_left
      (fun self -> function
        | `Type (id, tycon) ->
            insert id tycon self
        | _ ->
            self )
      M.empty AntStd.types
end

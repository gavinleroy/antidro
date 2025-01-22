open Lang

module Math = struct
  let plus = Identifier.var "+"
end

module Dom = struct
  let node = Identifier.var "Node"

  let nodety = VarT node

  let div_es = Identifier.var "div"

  let div_e_e = Identifier.var "div"

  let div_n = Identifier.var "div"

  let button_s_onclick = Identifier.var "button"

  let render_into = Identifier.var "render-into"
end

module Delta = struct
  module T = Set.Make (String)

  type t = T.t

  let initial = T.empty

  let add = T.add

  let union = T.union

  let mem = T.mem
end

module Gamma = struct
  module StringMap = Map.Make (String)

  type t =
    { (* alpha renaming map *)
      alpha: Identifier.t StringMap.t (* id -> type *)
    ; symbols: ty IdMap.t (* groupid -> overload list *)
    ; groups: methd list IdMap.t }

  let empty : t =
    {alpha= StringMap.empty; symbols= IdMap.empty; groups= IdMap.empty}

  let lookup (key : string) (self : t) =
    StringMap.find_opt key self.alpha
    |> Option.map (fun a ->
           (* NOTE, `a` is either the renamed value, or the method group id*)
           match IdMap.find_opt a self.symbols with
           | Some ty ->
               `Bound (a, ty)
           | None -> (
             match IdMap.find_opt a self.groups with
             | Some overloads ->
                 `Method (a, overloads)
             | None ->
                 `Unbound ) )
    |> Option.value ~default:`Unbound

  let mem (id : Identifier.t) (self : t) : bool = IdMap.mem id self.symbols

  let is_ref ((base, _) : Place.t) self =
    match base with
    | VarB sym ->
        IdMap.find_opt sym self.symbols
        |> Option.map Ty.is_ref
        |> Option.value ~default:false
    | _ ->
        failwith "internal error: looking up a non-variable base"

  let extend ?(raw_id = None) name ty self : Identifier.t * t =
    let id = Option.value raw_id ~default:(Identifier.var name) in
    ( id
    , { self with
        alpha= StringMap.add name id self.alpha
      ; symbols= IdMap.add id ty self.symbols } )

  let extend' raw_id name ty self : t =
    extend ~raw_id:(Some raw_id) name ty self |> snd

  let extendid id ty self : t = {self with symbols= IdMap.add id ty self.symbols}

  let extend_many bindings self : Identifier.t list * t =
    let rec loop ids acc = function
      | [] ->
          (List.rev ids, acc)
      | (name, ty) :: rest ->
          let id, acc = extend name ty acc in
          loop (id :: ids) acc rest
    in
    loop [] self bindings

  let add_method ?(raw_id = None) name signature self : Identifier.t * methd * t
      =
    let groupid, self =
      (* lookup the *string* `name` to see if the group exists, if not, create and add it *)
      match StringMap.find_opt name self.alpha with
      | None ->
          let newid = Identifier.var name in
          (newid, {self with alpha= StringMap.add name newid self.alpha})
      | Some id ->
          (id, self)
    in
    let classid = Identifier.var name
    and instanceid =
      Option.value raw_id ~default:(Identifier.var (name ^ "_instance"))
    in
    let methd = {id= instanceid; signature; group= groupid} in
    let groups =
      IdMap.find_opt groupid self.groups
      |> Option.value ~default:[] |> List.cons methd
      |> fun overloads -> IdMap.add groupid overloads self.groups
    in
    (classid, methd, {self with groups})

  let add_method' raw_id name sign self : t =
    add_method ~raw_id:(Some raw_id) name sign self |> fun (_, _, s) -> s

  let methods (i : Identifier.t) (self : t) : methd list =
    IdMap.find_opt i self.groups |> Option.value ~default:[]

  let initial : t =
    let nth_child_res n =
      Place.base Place.ResultB |> Place.offset n
      |> Place.slot (Slot.of_string "children")
    in
    empty
    |> extend' Math.plus "+"
         (let x = Identifier.var "x" and y = Identifier.var "y" in
          FnT
            (Signature.basic
               ~deps:
                 [DepsOn (Place.func_result, [Place.baseid x; Place.baseid y])]
               [(x, NumberT); (y, NumberT)]
               NumberT ) )
    |> add_method' Dom.div_e_e "div"
         (let c1 = Identifier.var "c1" and c2 = Identifier.var "c2" in
          Signature.basic
            ~deps:
              [ DepsOn (nth_child_res 0, [Place.baseid c1])
              ; DepsOn (nth_child_res 1, [Place.baseid c2]) ]
            [(c1, Dom.nodety); (c2, Dom.nodety)]
            Dom.nodety )
    |> add_method' Dom.div_n "div"
         (let n = Identifier.var "n" in
          Signature.basic
            ~deps:[DepsOn (nth_child_res 0, [Place.baseid n])]
            [(n, NumberT)]
            Dom.nodety )
    (* button :: String -> (Unit -> Unit^{}#{Write<!counter, !counter>}) -> Node *)
    |> add_method' Dom.button_s_onclick "button"
         (let n = Identifier.var "n" and onclick = Identifier.var "onclick" in
          Signature.basic
            ~deps:[DepsOn (nth_child_res 0, [Place.baseid n])]
            [(n, StringT); (onclick, FnT (Signature.polyall [] VoidT))]
            Dom.nodety )
    |> add_method' Dom.render_into "render-into"
         (let id = Identifier.var "id"
          and component = Identifier.var "component" in
          Signature.basic [(id, StringT); (component, Dom.nodety)] VoidT )
end

module Rho = struct
  module M = Map.Make (Place)
  module S = Set.Make (Place)

  type t = {forward: S.t M.t; reverse: S.t M.t}

  let show_set (s : S.t) : string =
    S.to_list s
    |> List.map (fun p -> Printf.sprintf "%s\n" (Place.show p))
    |> String.concat ", "

  let to_string ({forward= m; _} : t) : string =
    M.to_list m
    |> List.map (fun (k, v) ->
           Format.sprintf "%s -> %s@\n" (Place.show k) (show_set v) )
    |> String.concat ", \n"

  let initial = {forward= M.empty; reverse= M.empty}

  let extend (pl : Place.t) (deps : Place.t list) ({forward; reverse} : t) =
    let forward = M.add pl (S.of_list deps) forward in
    let reverse =
      List.fold_left
        (fun acc dep -> M.add dep (S.singleton pl) acc)
        reverse deps
    in
    {forward; reverse}

  let fold f ({forward; _} : t) acc = M.fold f forward acc

  let find_opt (pl : Place.t) ({forward; _} : t) = M.find_opt pl forward

  let combine rho1 rho2 =
    fold
      (fun pl deps acc ->
        match find_opt pl acc with
        | None ->
            extend pl (S.to_list deps) acc
        | Some deps' ->
            extend pl (S.union deps deps' |> S.to_list) acc )
      rho2 rho1

  let extendid id deps rho = extend (Place.baseid id) deps rho

  let depson pl iddep rho = extend pl [Place.baseid iddep] rho

  let depsonid id iddep rho = depson (Place.baseid id) iddep rho

  let withdeps (deps : dep list) (rho : t) : t =
    List.fold_left (fun rho (DepsOn (pl, deps)) -> extend pl deps rho) rho deps

  let roots_from (pl : Place.t) ({forward= rho; _} : t) : Place.t list =
    let rec loop (acc : S.t) (stack : S.t) =
      if S.is_empty stack then acc
      else
        let el = S.choose stack in
        let stack = S.remove el stack in
        match M.find_opt el rho with
        | None ->
            loop (S.add el acc) stack
        | Some deps ->
            loop acc (S.union deps stack)
    in
    loop S.empty (S.singleton pl) |> S.to_list

  let downstream_of (pl : Place.t) ({reverse= rho; _} : t) : Place.t list =
    let set_list_union l1 l2 =
      List.fold_left
        (fun acc el -> if List.mem el acc then acc else el :: acc)
        l1 l2
    in
    let rec loop acc = function
      | [] ->
          acc
      | el :: stack -> (
          let acc = set_list_union acc [el] in
          match M.find_opt el rho with
          | None ->
              loop acc stack
          | Some deps ->
              loop acc (S.to_list deps |> set_list_union acc) )
    in
    loop [] [pl]
end

type program =
  {expr: expr; ty: ty; rhos: (Rho.t IdMap.t[@sexp.opaque] [@opaque])}
[@@deriving sexp, show]

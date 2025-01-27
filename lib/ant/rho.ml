open Ty
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
    List.fold_left (fun acc dep -> M.add dep (S.singleton pl) acc) reverse deps
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

let withdeps (deps : Dep.t list) (rho : t) : t =
  List.fold_left (fun rho (pl, deps) -> extend pl deps rho) rho deps

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

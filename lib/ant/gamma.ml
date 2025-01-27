open Ty
module Map = Map.Make (Symbol)

type t = Ty.t Map.t

let lookup = Map.find_opt

let add = Map.add

(* module StringMap = Map.Make (String) *)
(* module Idmap = Map.Make (Symbol) *)
(* module GMap = Map.Make (Group) *)
(* type t = *)
(*   { alpha: Symbol.t StringMap.t *)
(*   ; symbols: Ty.t IdMap.t *)
(*   ; groups: PolyFnSig.t list GMap.t } *)

(* let empty : t = *)
(*   {alpha= StringMap.empty; symbols= IdMap.empty; groups= IdMap.empty} *)

(* let lookup (key : string) (self : t) = *)
(*   StringMap.find_opt key self.alpha *)
(*   |> Option.map (fun a -> *)
(*          (\* NOTE, `a` is either the renamed value, or the method group id*\) *)
(*          match IdMap.find_opt a self.symbols with *)
(*          | Some ty -> *)
(*              `Bound (a, ty) *)
(*          | None -> ( *)
(*            match IdMap.find_opt a self.groups with *)
(*            | Some overloads -> *)
(*                `Method (a, overloads) *)
(*            | None -> *)
(*                `Unbound ) ) *)
(*   |> Option.value ~default:`Unbound *)

(* let mem (id : Identifier.t) (self : t) : bool = IdMap.mem id self.symbols *)

(* let is_ref ((base, _) : Place.t) self = *)
(*   match base with *)
(*   | VarB sym -> *)
(*       IdMap.find_opt sym self.symbols *)
(*       |> Option.map Ty.is_ref *)
(*       |> Option.value ~default:false *)
(*   | _ -> *)
(*       failwith "internal error: looking up a non-variable base" *)

(* let extend ?(raw_id = None) name ty self : Identifier.t * t = *)
(*   let id = Option.value raw_id ~default:(Identifier.var name) in *)
(*   ( id *)
(*   , { self with *)
(*       alpha= StringMap.add name id self.alpha *)
(*     ; symbols= IdMap.add id ty self.symbols } ) *)

(* let extend' raw_id name ty self : t = *)
(*   extend ~raw_id:(Some raw_id) name ty self |> snd *)

(* let extendid id ty self : t = {self with symbols= IdMap.add id ty self.symbols} *)

(* let extend_many bindings self : Identifier.t list * t = *)
(*   let rec loop ids acc = function *)
(*     | [] -> *)
(*         (List.rev ids, acc) *)
(*     | (name, ty) :: rest -> *)
(*         let id, acc = extend name ty acc in *)
(*         loop (id :: ids) acc rest *)
(*   in *)
(*   loop [] self bindings *)

(* let add_method ?(raw_id = None) name signature self : Identifier.t * methd * t = *)
(*   let groupid, self = *)
(*     (\* lookup the *string* `name` to see if the group exists, if not, create and add it *\) *)
(*     match StringMap.find_opt name self.alpha with *)
(*     | None -> *)
(*         let newid = Identifier.var name in *)
(*         (newid, {self with alpha= StringMap.add name newid self.alpha}) *)
(*     | Some id -> *)
(*         (id, self) *)
(*   in *)
(*   let classid = Identifier.var name *)
(*   and instanceid = *)
(*     Option.value raw_id ~default:(Identifier.var (name ^ "_instance")) *)
(*   in *)
(*   let methd = {id= instanceid; signature; group= groupid} in *)
(*   let groups = *)
(*     IdMap.find_opt groupid self.groups *)
(*     |> Option.value ~default:[] |> List.cons methd *)
(*     |> fun overloads -> IdMap.add groupid overloads self.groups *)
(*   in *)
(*   (classid, methd, {self with groups}) *)

(* let add_method' raw_id name sign self : t = *)
(*   add_method ~raw_id:(Some raw_id) name sign self |> fun (_, _, s) -> s *)

(* let methods (i : Identifier.t) (self : t) : methd list = *)
(*   IdMap.find_opt i self.groups |> Option.value ~default:[] *)

(* let initial : t = *)
(*   let nth_child_res n = *)
(*     Place.base Place.ResultB |> Place.offset n *)
(*     |> Place.slot (Slot.of_string "children") *)
(*   in *)
(*   empty *)
(*   |> extend' Math.plus "+" *)
(*        (let x = Identifier.var "x" and y = Identifier.var "y" in *)
(*         FnT *)
(*           (Signature.basic *)
(*              ~deps:[DepsOn (Place.func_result, [Place.baseid x; Place.baseid y])] *)
(*              [(x, NumberT); (y, NumberT)] *)
(*              NumberT ) ) *)
(*   |> add_method' Dom.div_e_e "div" *)
(*        (let c1 = Identifier.var "c1" and c2 = Identifier.var "c2" in *)
(*         Signature.basic *)
(*           ~deps: *)
(*             [ DepsOn (nth_child_res 0, [Place.baseid c1]) *)
(*             ; DepsOn (nth_child_res 1, [Place.baseid c2]) ] *)
(*           [(c1, Dom.nodety); (c2, Dom.nodety)] *)
(*           Dom.nodety ) *)
(*   |> add_method' Dom.div_n "div" *)
(*        (let n = Identifier.var "n" in *)
(*         Signature.basic *)
(*           ~deps:[DepsOn (nth_child_res 0, [Place.baseid n])] *)
(*           [(n, NumberT)] *)
(*           Dom.nodety ) *)
(*   (\* button :: String -> (Unit -> Unit^{}#{Write<!counter, !counter>}) -> Node *\) *)
(*   |> add_method' Dom.button_s_onclick "button" *)
(*        (let n = Identifier.var "n" and onclick = Identifier.var "onclick" in *)
(*         Signature.basic *)
(*           ~deps:[DepsOn (nth_child_res 0, [Place.baseid n])] *)
(*           [(n, StringT); (onclick, FnT (Signature.polyall [] VoidT))] *)
(*           Dom.nodety ) *)
(*   |> add_method' Dom.render_into "render-into" *)
(*        (let id = Identifier.var "id" *)
(*         and component = Identifier.var "component" in *)
(*         Signature.basic [(id, StringT); (component, Dom.nodety)] VoidT ) *)

[@@@warning "-a"]

open Lang
open Ant.Ty
open Ant.Util

let dependency_graph (expr : expr) : Dependencies.t =
  let rec loop = function
    | VoidE | TrueE | FalseE | LitE _ | StringE _ | PlaceE _ | StructE _ ->
        Dependencies.empty
    | IfE (t, c, a) ->
        Dependencies.(loop t @ loop c @ loop a)
    | LetE {deps; body; _} ->
        Dependencies.(deps @ loop body)
    | LabelE {body; _} ->
        loop body
  in
  loop expr |> Dependencies.invert

let topological_edges (deps : Dep.t list) : (Place.t * Place.t) list =
  let edges =
    List.map Dep.destructure deps
    |> List.map (fun (pl, pls) -> List.map (fun pl' -> (pl, pl')) pls)
    |> List.flatten
  in
  let module PTbl = Hashtbl.Make (Place) in
  let module PSet = Set.Make (Place) in
  let graph = PTbl.create 50 and in_degree = PTbl.create 50 in
  let insert_edge f t =
    PTbl.add graph f
      (List.cons t (PTbl.find_opt graph f |> Option.value ~default:[]))
  in
  let bump_in_degree n =
    PTbl.add in_degree n
      ((PTbl.find_opt in_degree n |> Option.value ~default:0) + 1)
  in
  let dec_in_degree n = PTbl.add in_degree n (PTbl.find in_degree n - 1) in
  let nodes =
    List.fold_left
      (fun nodes (f, t) ->
        insert_edge f t ;
        bump_in_degree t ;
        PSet.add f (PSet.add t nodes) )
      PSet.empty edges
  in
  let zero_in_degree =
    PSet.filter
      (fun pl -> PTbl.find_opt in_degree pl |> Option.value ~default:0 = 0)
      nodes
    |> PSet.to_seq |> Queue.of_seq
  in
  let rec loop edges seen =
    Queue.take_opt zero_in_degree
    |> function
    | None ->
        if PSet.cardinal seen = PSet.cardinal nodes then List.rev edges
          (* FIXME: don't raise an error dumbass *)
        else raise (Failure "Downstream dependency Graph is not a DAG")
    | Some pl ->
        let neighbors = PTbl.find_opt graph pl |> Option.value ~default:[] in
        List.fold_left
          (fun edges pl' ->
            dec_in_degree pl' ;
            if PTbl.find in_degree pl' = 0 then Queue.add pl' zero_in_degree ;
            (pl, pl') :: edges )
          edges neighbors
        |> fun edges -> loop edges (PSet.add pl seen)
  in
  loop [] PSet.empty

let dependents_of (pl : Place.t) (expr : expr) : (Place.t * Place.t) list =
  let graph = dependency_graph expr in
  Logs.debug (fun m ->
      m "dependents_of %a graph: %a" Place.pp pl Dependencies.pp graph ) ;
  let find pl' = Dependencies.on pl' graph in
  let rec expand acc = function
    | [] ->
        acc
    | pl :: rest ->
        let pls = find pl in
        Logs.debug (fun m ->
            m "expanding %a -> %a" Place.pp pl (Format.pp_list Place.pp) pls ) ;
        expand (Dep.on pl pls :: acc) (pls @ rest)
  in
  expand [] [pl]
  |> fun deps ->
  Logs.debug (fun m -> m "expanded deps %a" (Format.pp_list Dep.pp) deps) ;
  deps |> topological_edges

let binding_struct (id : Symbol.t) (deps : Dependencies.t) :
    (Place.t * Ty.t) list =
  Dependencies.to_list deps
  |> List.filter_map (fun dep ->
         let pl, _pls = Dep.destructure dep in
         match Place.get_id_base pl with
         (* If this is an inner place and is based on `id`, there will be *)
         (* an updating function returned. *)
         | Some id' when Symbol.equal id id' && Place.is_inner pl ->
             Some (pl, Ty.void)
         | _ ->
             None )

let is_binder = function LetE _ | LabelE _ -> true | _ -> false

let rec update_place = Place.deref_to_slot

and update_expr expr =
  match expr with
  | VoidE | TrueE | FalseE | LitE _ | StringE _ | PlaceE _ | StructE _ | AppE _
    ->
      expr
  | IfE (t, c, a) ->
      IfE (update_expr t, update_expr c, update_expr a)
  | LabelE self ->
      LabelE
        {self with fbody= update_expr self.fbody; body= update_expr self.body}
  | LetE {bound; ty; deps; expr= SetE (pl, id); body} ->
      let pl = update_place pl in
      let deps = Dependencies.adjust update_place deps in
      LetE
        { bound
        ; ty
        ; deps
        ; expr= SetE (Place.slot Slot.box_value pl, id)
        ; body=
            LetE
              { bound= Symbol.fresh ()
              ; ty= Ty.void
              ; deps= Dependencies.empty
              ; expr= AppE {f= Place.slot Slot.broadcast pl; args= []}
              ; body= update_expr body } }
  | LetE
      { bound
      ; ty
      ; deps
      ; expr= RefE id
      ; body= LetE {bound= actually_bound; body; _} } ->
      let deps = Dependencies.adjust update_place deps in
      let body = update_expr body in
      let broadcast_bound = Symbol.fresh () in
      (* TODO: find all dependents of the value *)
      let to_update =
        dependents_of
          (Place.slot Slot.box_value (Place.baseid actually_bound))
          body
      in
      let updater_body =
        List.fold_right
          (fun (pl, pl') body ->
            let bound = Symbol.fresh () in
            let expr =
              if Place.is_inner pl' then
                AppE {f= Place.to_updater_id pl' |> Place.baseid; args= [bound]}
              else SetE (pl', bound)
            in
            LetE
              { bound
              ; ty= Ty.void
              ; deps= Dependencies.empty
              ; expr= PlaceE pl
              ; body=
                  LetE
                    { bound= Symbol.fresh ()
                    ; ty= Ty.void
                    ; deps= Dependencies.empty
                    ; expr
                    ; body } } )
          to_update VoidE
      in
      Logs.debug (fun m ->
          m "to_update dependencies %a"
            (Format.pp_list (fun fmt (pl, pl') ->
                 Format.fprintf fmt "%a -> %a" Place.pp pl Place.pp pl' ) )
            to_update ) ;
      Logs.debug (fun m -> m "Updating body with %a" pp_expr updater_body) ;
      LetE
        { bound= broadcast_bound
        ; ty= Ty.void
        ; deps= Dependencies.empty
        ; expr= FnE ([], updater_body)
        ; body=
            LetE
              { bound= actually_bound
              ; ty
              ; deps
              ; expr=
                  StructE
                    [(Slot.box_value, id); (Slot.broadcast, broadcast_bound)]
              ; body } }
  (* NOTE, we need to adjust the function return to reference the new *)
  (* returned struct type. The dependencies also need to  *)
  | FnE (sg, body) ->
      FnE (sg, update_expr body)
  (* IFF the RHS of the let is an application, then we expand out the *)
  (* return bindings to accommodate all returned inner updating functions *)
  | LetE {bound= id; ty; deps; expr= AppE data; body} ->
      Logs.debug (fun m -> m "Rewriting let binding for %a" Symbol.pp id) ;
      let (AppE data) = update_expr (AppE data) [@@warning "-8"] in
      let deps = Dependencies.adjust update_place deps in
      let body = update_expr body in
      let body =
        match body with
        | PlaceE pl ->
            StructE [(Slot.return, Place.get_id_base pl |> Option.get)]
        | _ when is_binder body ->
            body
        | _ ->
            assert false
      in
      let binding_fields = binding_struct id deps in
      let new_ty =
        Ty.struct_
          ( List.map
              (fun (pl, ty) -> (Place.to_updater_slot pl, ty))
              binding_fields
          |> List.cons (Slot.return, ty) )
      in
      (* The new original binder *)
      let struct_id = Symbol.derivative id in
      let struct_pl = Place.baseid struct_id in
      let body_with_updaters =
        List.fold_right
          (fun (pl, ty) body ->
            let id = Place.to_updater_id pl in
            let slot = Place.to_updater_slot pl in
            LetE
              { bound= id
              ; ty
              ; deps= Dependencies.empty
              ; expr= PlaceE (Place.slot slot struct_pl)
              ; body } )
          binding_fields body
      in
      LetE
        { bound= struct_id
        ; ty= new_ty
        ; deps= Dependencies.empty
        ; expr= AppE data
        ; body=
            LetE
              { bound= id
              ; ty
              ; deps
              ; expr= PlaceE (Place.slot Slot.return struct_pl)
              ; body= body_with_updaters } }
  (* IFF the BODY of the let is a place, and that place is the *)
  (* immediately bound variable, then this is the return *)
  | LetE {bound; ty; deps; expr; body= PlaceE pl} ->
      let expr = update_expr expr in
      let deps = Dependencies.adjust update_place deps in
      let body = Place.get_id_base pl |> Option.get in
      let structty = Ty.struct_ [(Slot.return, ty)] in
      let structe = StructE [(Slot.return, body)] in
      LetE {bound; ty= structty; deps; expr; body= structe}
  | LetE {bound; ty; deps; expr; body} ->
      let deps = Dependencies.adjust update_place deps in
      LetE {bound; ty; deps; expr= update_expr expr; body= update_expr body}
  | RefE _ | SetE _ | InlineJSE _ ->
      assert false

let run (p : program) : program = update_expr p

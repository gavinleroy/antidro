open Env
open Lang
open Ty

(* exception *)
(*   UnifyError of *)
(*     [ `OccursCheck of Identifier.t * ty *)
(*     | `MismatchedDependencies of dep list * dep list *)
(*     | `MismatchedEffects of eff list * eff list *)
(*     | `MismatchedTypes of ty * ty ] *)

(* let compare_ty = compare *)

(* let concrete_tys = [VoidT; BoolT; NumberT; StringT] *)

(* let is_concrete_ty x = List.mem x concrete_tys *)

(* let min_ty ty1 ty2 = if compare_ty ty1 ty2 <= 0 then ty1 else ty2 *)

(* (\* TODO: come back later and figure out the unionFind package *\) *)
(* module Ena = struct *)
(*   type t = *)
(*     (Identifier.t * ty) list *)
(*     * (Identifier.t * dep list) list *)
(*     * (Identifier.t * eff list) list *)

(*   let ty_subst ls = (ls, [], []) *)

(*   let append (tys1, deps1, effs1) (tys2, deps2, effs2) = *)
(*     (tys1 @ tys2, deps1 @ deps2, effs1 @ effs2) *)

(*   let flatten (ls : t list) : t = List.fold_right append ls ([], [], []) *)

(*   let rec subst_eff (s : eff list) (x : Identifier.t) (t : ty) : ty = *)
(*     let subst_signature s x (psig : polysig) : polysig = *)
(*       match psig.value.effects with *)
(*       | Opaque y when y = x -> *)
(*           {psig with value= {psig.value with effects= Transparent s}} *)
(*       | _ -> *)
(*           psig *)
(*     in *)
(*     match t with *)
(*     | ArrayT ty -> *)
(*         ArrayT (subst_eff s x ty) *)
(*     | StructT bindings -> *)
(*         StructT (List.map (fun (lbl, ty) -> (lbl, subst_eff s x ty)) bindings) *)
(*     | FnT psig -> *)
(*         FnT (subst_signature s x psig) *)
(*     | t -> *)
(*         t *)

(*   let rec subst_dep (s : dep list) (x : Identifier.t) (t : ty) : ty = *)
(*     let subst_signature s x (psig : polysig) : polysig = *)
(*       match psig.value.dependencies with *)
(*       | Opaque y when y = x -> *)
(*           {psig with value= {psig.value with dependencies= Transparent s}} *)
(*       | _ -> *)
(*           psig *)
(*     in *)
(*     match t with *)
(*     | ArrayT ty -> *)
(*         ArrayT (subst_dep s x ty) *)
(*     | StructT bindings -> *)
(*         StructT (List.map (fun (lbl, ty) -> (lbl, subst_dep s x ty)) bindings) *)
(*     | FnT psig -> *)
(*         FnT (subst_signature s x psig) *)
(*     | t -> *)
(*         t *)

(*   (\* substitute ty s for all occurrences of variable x in ty t *\) *)
(*   let rec subst (s : ty) (x : Identifier.t) (t : ty) : ty = *)
(*     let subst_signature s x (psig : polysig) : polysig = *)
(*       let subst_arg (id, ty) = (id, subst s x ty) in *)
(*       let subst_deps deps = deps in *)
(*       let subst_effs effs = effs in *)
(*       let sg = psig.value in *)
(*       { psig with *)
(*         value= *)
(*           { sg with *)
(*             args= List.map subst_arg sg.args *)
(*           ; return= subst s x sg.return *)
(*           ; dependencies= subst_deps sg.dependencies *)
(*           ; effects= subst_effs sg.effects } } *)
(*     in *)
(*     match t with *)
(*     | VarT id when id = x -> *)
(*         s *)
(*     | ArrayT ty -> *)
(*         ArrayT (subst s x ty) *)
(*     | StructT bindings -> *)
(*         StructT (List.map (fun (lbl, ty) -> (lbl, subst s x ty)) bindings) *)
(*     | FnT psig -> *)
(*         FnT (subst_signature s x psig) *)
(*     | t -> *)
(*         t *)

(*   (\* apply a substitution right to left *\) *)
(*   let apply ((stys, sdeps, seffs) : t) (t : ty) : ty = *)
(*     List.fold_right (fun (x, u) -> subst u x) stys t *)
(*     |> List.fold_right (fun (x, u) -> subst_dep u x) sdeps *)
(*     |> List.fold_right (fun (x, u) -> subst_eff u x) seffs *)

(*   let rec occurs (x : Identifier.t) (t : ty) : bool = *)
(*     let occurs_in_sg sg = *)
(*       List.exists (fun (_, ty) -> occurs x ty) sg.args || occurs x sg.return *)
(*     in *)
(*     match t with *)
(*     | VarT y -> *)
(*         x = y *)
(*     | ArrayT ty -> *)
(*         occurs x ty *)
(*     | StructT bindings -> *)
(*         List.exists (fun (_, ty) -> occurs x ty) bindings *)
(*     | FnT psig -> *)
(*         psig.value |> occurs_in_sg *)
(*     | _ -> *)
(*         false *)

(*   (\* unify one pair *\) *)
(*   let rec unify_one (s : ty) (t : ty) : t = *)
(*     match (s, t) with *)
(*     | VoidT, VoidT | BoolT, BoolT | NumberT, NumberT | StringT, StringT -> *)
(*         ty_subst [] *)
(*     | VarT x, VarT y when x = y -> *)
(*         ty_subst [] *)
(*     | VarT x, VarT _ -> *)
(*         ty_subst [(x, t)] *)
(*     | ArrayT s, ArrayT t -> *)
(*         unify_one s t *)
(*     | StructT bn1, StructT bn2 -> *)
(*         List.map *)
(*           (fun (sl, s) -> *)
(*             let t = List.assoc sl bn2 in *)
(*             unify_one s t ) *)
(*           bn1 *)
(*         |> flatten *)
(*     | FnT sg1, FnT sg2 -> ( *)
(*         let sg1 = Signature.skip_binders sg1 in *)
(*         let sg2 = Signature.skip_binders sg2 in *)
(*         List.map2 (fun (_, ty1) (_, ty2) -> unify_one ty1 ty2) sg1.args sg2.args *)
(*         |> flatten *)
(*         |> append (unify_one sg1.return sg2.return) *)
(*         |> fun (tys, deps, effs) -> *)
(*         match (sg1.dependencies, sg2.dependencies) with *)
(*         | Opaque x, Opaque y when x = y -> *)
(*             (tys, deps, effs) *)
(*         | Opaque _, Opaque _ -> *)
(*             failwith "NYI: unification of opaque dependencies" *)
(*         | Transparent x, Transparent y -> *)
(*             if List.for_all (fun el -> List.mem el y) x then (tys, deps, effs) *)
(*             else raise (UnifyError (`MismatchedDependencies (x, y))) *)
(*         | Transparent x, Opaque y | Opaque y, Transparent x -> ( *)
(*             (tys, (y, x) :: deps, effs) *)
(*             |> fun (tys, deps, effs) -> *)
(*             match (sg1.effects, sg2.effects) with *)
(*             | Opaque x, Opaque y when x = y -> *)
(*                 (tys, deps, effs) *)
(*             | Opaque _, Opaque _ -> *)
(*                 failwith "NYI: unification of opaque effects" *)
(*             | Transparent x, Transparent y -> *)
(*                 if x = y then (tys, deps, effs) *)
(*                 else raise (UnifyError (`MismatchedEffects (x, y))) *)
(*             | Transparent x, Opaque y | Opaque y, Transparent x -> *)
(*                 (tys, deps, (y, x) :: effs) ) ) *)
(*     | VarT x, t | t, VarT x -> *)
(*         if occurs x t then raise (UnifyError (`OccursCheck (x, t))) *)
(*         else ty_subst [(x, t)] *)
(*     | ty1, ty2 -> *)
(*         raise (UnifyError (`MismatchedTypes (ty1, ty2))) *)

(*   (\* unify a list of pairs *\) *)
(*   let rec unify ss : t = *)
(*     match ss with *)
(*     | [] -> *)
(*         ([], [], []) *)
(*     | (x, y) :: t -> *)
(*         let t2 = unify t in *)
(*         let t1 = unify_one (apply t2 x) (apply t2 y) in *)
(*         append t1 t2 *)
(* end *)

(* let unify_candidate polysig args tys = *)
(*   let sg1 = FnT polysig in *)
(*   let sg2 = *)
(*     FnT {polysig with value= {polysig.value with args= List.combine args tys}} *)
(*   in *)
(*   try *)
(*     let subst = Ena.unify_one sg1 sg2 in *)
(*     Ena.apply subst sg1 *)
(*     |> (function FnT sg -> sg | _ -> assert false) *)
(*     |> Signature.skip_binders |> Option.some *)
(*   with _ -> None *)

(* (\* Instantite the signature with the given tys and argument ids (args). *\) *)
(* (\* This should return the 1. return type, 2. result dependencies, and 3. resulting effects. *\) *)
(* let instantiate polysig args tys resultid = *)
(*   unify_candidate polysig args tys *)
(*   |> Option.map (fun applied -> *)
(*          let depssubst = *)
(*            (Place.ResultB, resultid) *)
(*            :: List.combine *)
(*                 (List.map (fun a -> Place.VarB (fst a)) applied.args) *)
(*                 args *)
(*          in *)
(*          let sb = Place.subst depssubst in *)
(*          let dependencies = *)
(*            List.map *)
(*              (fun (DepsOn (base, pls)) -> *)
(*                (base, DepsOn (sb base, List.map sb pls)) ) *)
(*              ( applied.dependencies *)
(*              |> function Transparent ls -> ls | _ -> assert false ) *)
(*          in *)
(*          let effects = *)
(*            List.map *)
(*              (fun (WriteEf pl) -> WriteEf (sb pl)) *)
(*              ( applied.effects *)
(*              |> function Transparent ls -> ls | Opaque _ -> assert false ) *)
(*          in *)
(*          (applied.return, dependencies, effects) ) *)

(* let freevars formals body : Identifier.t list = *)
(*   let module IdSet = Set.Make (Identifier) in *)
(*   let add_maybe acc = function Some id -> IdSet.add id acc | None -> acc in *)
(*   let rec loop acc = function *)
(*     | VoidE | TrueE | FalseE | LitE _ | StringE _ -> *)
(*         acc *)
(*     | NewE (id, freevars) -> *)
(*         IdSet.add_seq (id :: freevars |> List.to_seq) acc *)
(*     | PlaceE pl -> *)
(*         Place.get_id_base pl |> add_maybe acc *)
(*     | LetE (id, _, e1, e2) -> *)
(*         loop acc e2 |> IdSet.remove id |> fun acc -> loop acc e1 *)
(*     | StructE bindings -> *)
(*         List.map snd bindings |> List.to_seq |> fun seq -> IdSet.add_seq seq acc *)
(*     | FnE (_, e) -> *)
(*         loop acc e *)
(*     | AppE (aid, args) -> *)
(*         aid :: args |> List.to_seq |> fun seq -> IdSet.add_seq seq acc *)
(*     | SetE (pl, id) -> *)
(*         Place.get_id_base pl |> add_maybe acc |> IdSet.add id *)
(*   in *)
(*   let used_in_body = loop IdSet.empty body in *)
(*   List.map fst formals |> IdSet.of_list |> IdSet.diff used_in_body *)
(*   |> IdSet.filter (fun el -> Gamma.mem el Gamma.initial |> not) *)
(*   |> IdSet.elements *)

(* let resolve_application gamma fty ids tys : (Signature.t, 'a) result = *)
(*   let apply_signature (psig : PolyFnSig.t) = *)
(*     unify_candidate psig ids tys *)
(*     |> Option.map (fun _ -> true) *)
(*     |> Option.value ~default:false *)
(*   in *)
(*   match fty with *)
(*   | FnT s -> *)
(*       apply_signature s *)
(*   | MethodGroupT groupid -> ( *)
(*       let ms = Gamma.methods groupid gamma in *)
(*       ms *)
(*       |> List.filter_map (fun m -> apply_signature m.signature *)
(*                                |> fun _ -> failwith "NYI") *)
(*       |> function *)
(*       | [] -> *)
(*           err (`NoMethodApplicable (ms, tys)) *)
(*       | [m] -> *)
(*           cont (`MethodApp m) *)
(*       | apps -> *)
(*           err (`AmbiguousMethodApp (apps, tys)) ) *)
(*   | _ -> *)
(*       assert false *)

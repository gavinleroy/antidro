module TLang = Typeck.Lang
open Ant.Ty
open Lang
module AntPrim = AntStd.Prim

module Blocks : sig
  val shift : unit -> unit

  val unshift : unit -> stmt list

  val push : stmt -> unit

  val is_empty : unit -> bool
end = struct
  let bodies : stmt list list ref = ref []

  let shift () = bodies := [] :: !bodies

  let unshift () =
    let b = List.hd !bodies |> List.rev in
    bodies := List.tl !bodies ;
    b

  let push stmt = bodies := (stmt :: List.hd !bodies) :: List.tl !bodies

  let is_empty () = List.is_empty !bodies
end

let id_to_e x = PlaceE (Place.baseid x)

let access_app_return e =
  let sym = Symbol.fresh () in
  Blocks.push (LetS (sym, e)) ;
  PlaceE (Place.slot Slot.return (Place.baseid sym))

let return_expr e = StructE [(Slot.return, e)]

let rec doplace pl = pl

and doexpr : TLang.expr -> expr = function
  | TLang.VoidE ->
      VoidE
  | TLang.TrueE ->
      TrueE
  | TLang.FalseE ->
      FalseE
  | TLang.LitE i ->
      AppE (PlaceE (Place.baseid AntStd.wrap), [LitE i]) |> access_app_return
  | TLang.StringE s ->
      AppE (PlaceE (Place.baseid AntStd.wrap), [StringE s]) |> access_app_return
  | TLang.PlaceE pl ->
      PlaceE (doplace pl)
  | TLang.LetE {bound; expr= TLang.IfE (c, t, e); body; _} ->
      let c = doexpr c in
      Blocks.shift () ;
      Blocks.push (LetS (bound, doexpr t)) ;
      let t = Blocks.unshift () in
      Blocks.shift () ;
      Blocks.push (LetS (bound, doexpr e)) ;
      let e = Blocks.unshift () in
      Blocks.push (IfS (c, t, e)) ;
      doexpr body
  | TLang.IfE (_c, _t, _e) ->
      failwith "if-expressions should have been desugared"
  | TLang.LetE {bound; expr; body; _} ->
      Blocks.push (LetS (bound, doexpr expr)) ;
      doexpr body
  | TLang.RefE x ->
      AppE (PrimE AntPrim.ObservableP, [id_to_e x])
  | TLang.DerefE x ->
      (* TODO *)
      id_to_e x
  | TLang.StructE fields ->
      StructE (List.map (fun (sl, x) -> (sl, id_to_e x)) fields)
  | TLang.SetE (pl, x) ->
      SetE (doplace pl, id_to_e x)
  | LabelE {overloadid; args; fbody; body; _} ->
      Blocks.push (LetS (overloadid, make_lambda args fbody)) ;
      doexpr body
  | TLang.FnE (formals, body) ->
      make_lambda formals body
  | TLang.AppE {f; args} ->
      AppE (PlaceE (doplace f), List.map id_to_e args) |> access_app_return

and make_lambda formals body =
  Blocks.shift () ;
  Blocks.push (ReturnS (return_expr (doexpr body))) ;
  FnE (formals, Blocks.unshift ())

let run (p : TLang.program) : program =
  Blocks.shift () ;
  Blocks.push (ReturnS (doexpr p)) ;
  let prog = Blocks.unshift () in
  assert (Blocks.is_empty ()) ;
  prog

let sexp_of_program = sexp_of_program

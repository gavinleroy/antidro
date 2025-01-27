include Lang

let push_stmt, shift_stmts, pop_stmts, is_empty =
  let stmts = ref [] in
  ( (fun stmt ->
      match !stmts with
      | hd :: rest ->
          stmts := (stmt :: hd) :: rest
      | _ ->
          failwith "ICE: push_stmt" )
  , (fun () -> stmts := [] :: !stmts)
  , (fun () ->
      match !stmts with
      | hd :: res ->
          stmts := res ;
          List.rev hd
      | _ ->
          failwith "ICE: pop_stmts" )
  , fun () -> List.is_empty !stmts )

let push_class, get_classes =
  let classes = ref [] in
  ( (fun (name, psig, body) -> classes := {name; psig; body} :: !classes)
  , fun () -> List.rev !classes )

let rec lower_place pl = pl

and lower_expr expr =
  match expr with
  | Prev.VoidE ->
      VoidE
  | Prev.TrueE ->
      TrueE
  | Prev.FalseE ->
      FalseE
  | Prev.LitE i ->
      LitE i
  | Prev.StringE s ->
      StringE s
  | Prev.PlaceE pl ->
      let pl = lower_place pl in
      PlaceE pl
  | Prev.InlineJS js ->
      InlineJS js
  | Prev.NewE (id, args) ->
      NewE (id, args)
  (* SPECIAL: lift function expressions to the top-level *)
  | Prev.LetE (id, Typeck.UncallableT, Prev.FnE (fs, fbody), body) ->
      shift_stmts () ;
      let fbody = lower_expr fbody in
      push_stmt (ReturnS fbody) ;
      let sbody = pop_stmts () in
      push_class (id, fs, sbody) ;
      lower_expr body
  | Prev.LetE (_, _, Prev.SetE (pl, id), body) ->
      push_stmt (SetS (pl, id)) ;
      lower_expr body
  | Prev.LetE (id, _, e1, body) ->
      let e1 = lower_expr e1 in
      push_stmt (LetS (id, e1)) ;
      lower_expr body
  | Prev.StructE bindings ->
      StructE bindings
  | Prev.AppE (id, args) ->
      AppE (id, args)
  | Prev.SetE (pl, id) ->
      push_stmt (SetS (pl, id)) ;
      VoidE
  | Prev.LamE (id, body) ->
      shift_stmts () ;
      let body = lower_expr body in
      push_stmt (ReturnS body) ;
      let sbody = pop_stmts () in
      LamE (id, sbody)
  (* NOTE: these should have already been covered, if not, the *)
  (* program structure invariants coming out of typeck may have changed. *)
  | Prev.FnE _ ->
      Printf.eprintf "ICE: lower_expr: %s\n"
        (Prev.sexp_of_expr expr |> Sexplib.Sexp.to_string_hum) ;
      failwith "Function expressions should have been desugared"

and lower_program ({expr; ty; _} : Prev.program) : program =
  shift_stmts () ;
  let body = lower_expr expr in
  push_stmt (ReturnS body) ;
  let body = pop_stmts () in
  let entry = Identifier.var "main" in
  push_class (entry, Signature.basic [] ty, body) ;
  assert (is_empty ()) ;
  {entry= Some entry; classes= get_classes ()}

let run p = lower_program p

let sexp_of_program = sexp_of_program

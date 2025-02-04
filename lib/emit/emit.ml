open Ant.Ty
open Typeck.Lang
open Format

let pp_comma fmt () = fprintf fmt ",@ "

let emit_place = Place.pp

(* let emit_formals fmt args = *)
(*   pp_print_list ~pp_sep:pp_comma Identifier.pp fmt args *)

(* let emit_args fmt args = pp_print_list ~pp_sep:pp_comma Identifier.pp fmt args *)

let is_let_binding = function LetE _ | LabelE _ -> true | _ -> false

(* NOTE: don't move me pls *)
let rec emit_expr fmt expr =
  match expr with
  | VoidE ->
      fprintf fmt "void(0)"
  | TrueE ->
      fprintf fmt "true"
  | FalseE ->
      fprintf fmt "false"
  | LitE i ->
      fprintf fmt "%d" i
  | StringE s ->
      fprintf fmt "%S" s
  | PlaceE pl ->
      emit_place fmt pl
  | IfE (e1, e2, e3) ->
      fprintf fmt "(%a ? (%a) : (%a))" emit_expr e1 emit_expr e2 emit_expr e3
  | InlineJSE s ->
      fprintf fmt "%s" s
  | RefE e ->
      fprintf fmt "{ %a: %a }" Slot.pp Slot.box_value Symbol.pp e
  | SetE (pl, e) ->
      fprintf fmt "(%a = %a)" emit_place pl Symbol.pp e
  | StructE fields ->
      fprintf fmt "{ %a }" (pp_print_list ~pp_sep:pp_comma emit_field) fields
  | AppE {f; args; _} ->
      fprintf fmt "%a(%a)" Place.pp f
        (pp_print_list ~pp_sep:pp_comma Symbol.pp)
        args
  (* Functions *)
  | LabelE {overloadid= id; args; fbody= body; body= rest; _} ->
      let args = List.map fst args in
      fprintf fmt "var %a = (%a) => { %a };@\n%a" Symbol.pp id
        (pp_print_list ~pp_sep:pp_comma Symbol.pp)
        args emit_expr body emit_expr rest
  (* Returns *)
  | LetE {bound; expr; body; _} when not (is_let_binding body) ->
      fprintf fmt "var %a = %a; return %a" Symbol.pp bound emit_expr expr
        emit_expr body
  (* other *)
  | LetE {bound; expr; body; _} ->
      fprintf fmt "var %a = %a; %a" Symbol.pp bound emit_expr expr emit_expr
        body
  | FnE (formals, body) ->
      fprintf fmt "(%a) => { %a }"
        (pp_print_list ~pp_sep:pp_comma Symbol.pp)
        (List.map fst formals) emit_expr body

and emit_field fmt (sl, expr) = fprintf fmt "%a: %a" Slot.pp sl Symbol.pp expr

let run ~out (expr : program) =
  let fmt = formatter_of_out_channel out in
  emit_expr fmt expr

open Ant.Ty
open Ant.Util
open Air.Lang
open Format

let emit_place = Place.pp

(* NOTE: don't move me pls *)
let rec emit_stmt fmt = function
  | LetS (bound, expr) ->
      fprintf fmt "var %a = %a;" Symbol.pp bound emit_expr expr
  | IfS (c, t, e) ->
      fprintf fmt "if (%a) { %a } else { %a }" emit_expr c emit_body t emit_body
        e
  | ReturnS expr ->
      fprintf fmt "return %a;" emit_expr expr
  | JSBody s ->
      fprintf fmt "%s" s

and emit_expr fmt expr =
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
  | PrimE p ->
      fprintf fmt "%a" AntStd.Prim.pp p
  | SetE (pl, e) ->
      fprintf fmt "(%a = %a)" emit_place pl emit_expr e
  | StructE fields ->
      fprintf fmt "{ %a }" (pp_print_list ~pp_sep:pp_comma emit_field) fields
  | AppE (f, args) ->
      fprintf fmt "(%a)(%a)" emit_expr f
        (pp_print_list ~pp_sep:pp_comma emit_expr)
        args
  | FnE (formals, body) ->
      fprintf fmt "(%a) => { %a }"
        (pp_print_list ~pp_sep:pp_comma Symbol.pp)
        (List.map fst formals) emit_body body

and emit_field fmt (sl, expr) = fprintf fmt "%a: %a" Slot.pp sl emit_expr expr

and emit_body fmt = Format.pp_print_list ~pp_sep:pp_print_cut emit_stmt fmt

and emit_program fmt prog =
  let emit_prologue fmt () =
    fprintf fmt
      "import { observable, autorun, action, computed, reaction } from 'mobx';"
  in
  let entry = Symbol.var "antidro_toplevel" in
  Format.fprintf fmt "%a@\n%a@\n%a()@." emit_prologue () emit_stmt
    (LetS (entry, FnE ([], prog)))
    Symbol.pp entry

let run ~out (p : program) =
  let fmt = formatter_of_out_channel out in
  emit_program fmt p

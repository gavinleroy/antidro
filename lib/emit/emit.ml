open Ant
open Format

let pp_comma fmt () = fprintf fmt ",@ "

let emit_place fmt ((base, adjs) : Place.t) =
  let emit_base fmt = function
    | Place.VarB id ->
        fprintf fmt "%a" Identifier.pp id
    | Place.ResultB ->
        failwith "Result base should only exist at the type level"
  and emit_adj fmt = function
    | Place.SplatAdj ->
        failwith "Splat adjustment should only exist at the type level"
    | Place.SlotAdj i ->
        fprintf fmt ".%a" Slot.pp i
    | Place.OffAdj i ->
        fprintf fmt "[%d]" i
    | Place.DynOffAdj i ->
        fprintf fmt "[%a]" Identifier.pp i
  in
  fprintf fmt "%a%a" emit_base base (pp_print_list emit_adj) adjs

let emit_formals fmt args =
  pp_print_list ~pp_sep:pp_comma Identifier.pp fmt args

let emit_args fmt args = pp_print_list ~pp_sep:pp_comma Identifier.pp fmt args

(* NOTE: don't move me pls *)
let rec emit_expr fmt = function
  | VoidE ->
      fprintf fmt "void(0)"
  | TrueE ->
      fprintf fmt "true"
  | FalseE ->
      fprintf fmt "false"
  | LitE i ->
      fprintf fmt "%d" i
  | StringE s ->
      fprintf fmt "\"%s\"" s
  | InlineJS js ->
      pp_print_text fmt js
  | PlaceE pl ->
      emit_place fmt pl
  | StructE fields ->
      let emit_field fmt (sl, id) =
        fprintf fmt "%a: %a" Slot.pp sl Identifier.pp id
      in
      fprintf fmt "{@[<hov 2>%a@]}"
        (pp_print_list ~pp_sep:pp_comma emit_field)
        fields
  | NewE (id, args) ->
      fprintf fmt "(new %a(%a))" Identifier.pp id emit_args args
  | LamE (args, body) ->
      fprintf fmt "(%a) => {@[<hov 2>%a@]}" emit_formals args emit_body body
  | AppE (id, args) ->
      fprintf fmt "%a(%a)" Identifier.pp id emit_args args

and emit_stmt fmt = function
  | LetS (id, expr) ->
      fprintf fmt "var %a = %a;@," Identifier.pp id emit_expr expr
  | SetS (pl, id) ->
      fprintf fmt "%a = %a;@," emit_place pl Identifier.pp id
  | ReturnS expr ->
      fprintf fmt "return (%a);@," emit_expr expr

and emit_body fmt stmts =
  pp_print_list ~pp_sep:pp_print_space emit_stmt fmt stmts

let emit_class fmt {name; body; psig} =
  let inner_id = Identifier.var "inner"
  and upvars = Signature.upvars psig
  and args = Signature.args psig in
  let emit_inner fmt () =
    fprintf fmt "function %a(%a) {@[<hov 2>%a@]}" Identifier.pp inner_id
      emit_formals args emit_body body
  in
  let emit_constructor fmt () =
    fprintf fmt
      "constructor(%a) {@[<hov 2>%a Object.setPrototypeOf(%a, %a.prototype); \
       return %a;@]}"
      emit_formals upvars emit_inner () Identifier.pp inner_id Identifier.pp
      name Identifier.pp inner_id
  in
  fprintf fmt "class %a extends Function {@,@[<hov 2>%a@]}" Identifier.pp name
    emit_constructor ()

let emit_classes fmt classes =
  fprintf fmt "%a" (pp_print_list ~pp_sep:pp_print_newline emit_class) classes

let run ~out ({classes; entry} : program) =
  let emit_entry fmt = function
    | Some id ->
        fprintf fmt "(new %a())();" Identifier.pp id
    | None ->
        ()
  in
  let fmt = formatter_of_out_channel out in
  fprintf fmt "%a%a@?" emit_classes classes emit_entry entry

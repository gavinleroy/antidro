module Sexp = Sexplib.Sexp
open Sexplib.Std

type 'a stx = {value: 'a; sexp: Sexp.t}

type number = int

and symbol = string

and ty_binding = symbol * ty

and formals = ty_binding list

and decl = Def of (symbol * expr) | Defn of (symbol * formals * body)
[@@deriving sexp, show]

and dep = DepsOn of place * place list [@@deriving sexp, show]

and eff = WriteE of place list * place [@@deriving sexp, show]

and ty =
  | VarT of symbol
  | RefT of ty
  | ArrayT of ty
  | StructT of ty_binding list
  | FnT of formals * ty * dep list * eff list
[@@deriving sexp, show]

and place = VarP of symbol | SlotP of place * symbol | ArefP of place * symbol
[@@deriving sexp, show]

and body = decl list * expr [@@deriving sexp, show]

and expr =
  | TrueE
  | FalseE
  | LitE of number
  | StringE of string
  | PlaceE of place
  | IfE of expr * expr * expr
  | BeginE of expr list
  | StructE of (symbol * expr) list
  | FnE of formals * body
  | AppE of symbol * expr list
  | SetE of place * expr
  | RefE of expr
  | DerefE of expr
[@@deriving sexp, show]

and program = body [@@deriving sexp, show]

type err_cont = Sexp.t -> string -> program

type 'a continuation = 'a -> program

type 'a parser = Sexp.t -> err_cont -> 'a continuation -> program

type 'a many_parser = Sexp.t list -> err_cont -> 'a continuation -> program

let is_string s =
  1 < String.length s && s.[0] = '\'' && s.[String.length s - 1] = '\''

let is_numeric s = int_of_string_opt s |> Option.is_some

let is_symbol s =
  match s.[0] with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

let parse_many_with :
    Sexp.t list -> 'a parser -> ('a list * Sexp.t list) continuation -> program
    =
 fun sexps f cont ->
  let rec loop acc = function
    | [] ->
        cont (List.rev acc, [])
    | sexp :: rest ->
        f sexp
          (fun sexp emsg ->
            Logs.debug (fun m ->
                m "parse_many_with halted on %a: %s" Sexp.pp_hum sexp emsg ) ;
            cont (List.rev acc, sexp :: rest) )
          (fun parsed -> loop (parsed :: acc) rest)
  in
  loop [] sexps

let rec parse_symbol : symbol parser =
 fun sexp err cont ->
  let open Sexp in
  match sexp with Atom name -> cont name | _ -> err sexp "expected symbol"

and parse_symbols : symbol list many_parser =
 fun sexps err cont ->
  parse_many_with sexps parse_symbol (function
    | symbs, [] ->
        cont symbs
    | _, who :: _ ->
        err who "expected end of symbols" )

and parse_ty : ty parser =
 fun sexp err cont ->
  let open Sexp in
  match sexp with
  | Atom name ->
      cont (VarT name)
  | List [Atom "Array"; ty] ->
      parse_ty ty err (fun ty -> cont (ArrayT ty))
  | List [Atom "Ref"; ty] ->
      parse_ty ty err (fun ty -> cont (RefT ty))
  | List [Atom "struct"; List fields] ->
      parse_bindings fields err (fun fields -> cont (StructT fields))
  (* (-> formals return_ty) *)
  | List [Atom "->"; List formals; ty] ->
      parse_formals formals err (fun formals ->
          parse_ty ty err (fun ty -> cont (FnT (formals, ty, [], []))) )
  (* (-> formals return_ty deps) *)
  | List [Atom "->"; List formals; ty; List deps] ->
      parse_formals formals err (fun formals ->
          parse_ty ty err (fun ty ->
              parse_deps deps err (fun deps ->
                  cont (FnT (formals, ty, deps, [])) ) ) )
  (* (-> formals return_ty deps effs) *)
  | List [Atom "->"; List formals; ty; List deps; List effs] ->
      parse_formals formals err (fun formals ->
          parse_ty ty err (fun ty ->
              parse_deps deps err (fun deps ->
                  parse_effs effs err (fun effs ->
                      cont (FnT (formals, ty, deps, effs)) ) ) ) )
  | _ ->
      err sexp "expected type"

and parse_dep : dep parser =
 fun sexp err cont ->
  let open Sexp in
  match sexp with
  | List [Atom "^"; place; List places] ->
      parse_place place err (fun place ->
          parse_places places err (fun places -> cont (DepsOn (place, places))) )
  | _ ->
      err sexp "expected dependency"

and parse_eff : eff parser =
 fun sexp err cont ->
  let open Sexp in
  match sexp with
  | List [Atom "write"; List places; place] ->
      parse_places places err (fun places ->
          parse_place place err (fun place -> cont (WriteE (places, place))) )
  | _ ->
      err sexp "expected effect"

and parse_deps : dep list many_parser =
 fun sexps err cont ->
  parse_many_with sexps parse_dep (function
    | deps, [] ->
        cont deps
    | _, who :: _ ->
        err who "expected end of dependencies" )

and parse_effs : eff list many_parser =
 fun sexps err cont ->
  parse_many_with sexps parse_eff (function
    | effs, [] ->
        cont effs
    | _, who :: _ ->
        err who "expected end of effects" )

and parse_ty_binding : ty_binding parser =
 fun sexp err cont ->
  let open Sexp in
  match sexp with
  | List [Atom name; ty] ->
      parse_ty ty err (fun ty -> cont (name, ty))
  | _ ->
      err sexp "expected formal binding"

and parse_bindings : ty_binding list many_parser =
 fun sexps err cont ->
  parse_many_with sexps parse_ty_binding (function
    | bindings, [] ->
        cont bindings
    | _, who :: _ ->
        err who "expected end of formals" )

and parse_formals sexp err cont =
  (* no fancy formals allowed yet *)
  parse_bindings sexp err cont

and parse_place : place parser =
 fun sexp err cont ->
  let open Sexp in
  match sexp with
  | Atom name ->
      cont (VarP name)
  | List [Atom "slot"; place; Atom name] ->
      parse_place place err (fun place -> cont (SlotP (place, name)))
  | List [Atom "aref"; place; Atom name] ->
      parse_place place err (fun place -> cont (ArefP (place, name)))
  | _ ->
      err sexp "expected place"

and parse_places : place list many_parser =
 fun sexps err cont ->
  parse_many_with sexps parse_place (function
    | places, [] ->
        cont places
    | _, who :: _ ->
        err who "expected end of places" )

and parse_decl : decl parser =
 fun sexp err cont ->
  let open Sexp in
  match sexp with
  | List (Atom "defn" :: Atom name :: List formals :: body) ->
      parse_formals formals err (fun formals ->
          parse_body body err (fun body -> cont (Defn (name, formals, body))) )
  | List [Atom "def"; Atom name; expr] ->
      parse_expr expr err (fun expr -> cont (Def (name, expr)))
  | _ ->
      err sexp "expected def or defn"

and parse_simple_expr : expr parser =
 fun sexp err cont ->
  let open Sexp in
  match sexp with
  | Atom "true" ->
      cont TrueE
  | Atom "false" ->
      cont FalseE
  | Atom a when is_string a ->
      cont (StringE (String.sub a 1 (String.length a - 2)))
  | Atom a when int_of_string_opt a |> Option.is_some ->
      cont (LitE (int_of_string a))
  | place ->
      parse_place place err (fun place -> cont (PlaceE place))

and parse_expr : expr parser =
 fun sexp err cont ->
  let open Sexp in
  let parse_complex_expr _ _ =
    match sexp with
    | List [Atom "!"; place] ->
        parse_place place err (fun place -> cont (DerefE (PlaceE place)))
    | List [Atom "if"; test; consequent; alternate] ->
        parse_expr test err (fun test ->
            parse_expr consequent err (fun consequent ->
                parse_expr alternate err (fun alternate ->
                    cont (IfE (test, consequent, alternate)) ) ) )
    (* desugar into an "if" *)
    | List (Atom "or" :: sexps) ->
        parse_exprs sexps err (fun exprs ->
            List.fold_right (fun ex acc -> IfE (ex, TrueE, acc)) exprs FalseE
            |> cont )
    | List (Atom "and" :: sexprs) ->
        parse_exprs sexprs err (fun exprs ->
            List.fold_right (fun ex acc -> IfE (ex, acc, FalseE)) exprs TrueE
            |> cont )
    | List [Atom "not"; expr] ->
        parse_expr expr err (fun expr -> cont (IfE (expr, FalseE, TrueE)))
    | List [Atom "struct"; List bindings] ->
        parse_field_bindings bindings err (fun bindings ->
            cont (StructE bindings) )
    | List (Atom "begin" :: sexps) ->
        parse_exprs sexps err (fun exprs -> cont (BeginE exprs))
    | List (Atom "fn" :: List formals :: body) ->
        parse_formals formals err (fun formals ->
            parse_body body err (fun body -> cont (FnE (formals, body))) )
    | List [Atom "set!"; place; expr] ->
        parse_place place err (fun place ->
            parse_expr expr err (fun expr -> cont (SetE (place, expr))) )
    | List [Atom "ref"; expr] ->
        parse_expr expr err (fun expr -> cont (RefE expr))
    | List (f :: args) ->
        Logs.debug (fun m -> m "Parsing %a as application" Sexp.pp_hum sexp) ;
        parse_symbol f err (fun f ->
            parse_exprs args err (fun args -> cont (AppE (f, args))) )
    | _ ->
        err sexp "expected expression"
  in
  parse_simple_expr sexp parse_complex_expr cont

and parse_field_binding : (symbol * expr) parser =
 fun sexp err cont ->
  let open Sexp in
  match sexp with
  | List [Atom name; expr] ->
      parse_expr expr err (fun expr -> cont (name, expr))
  | _ ->
      err sexp "expected field binding"

and parse_field_bindings : (symbol * expr) list many_parser =
 fun sexps err cont ->
  parse_many_with sexps parse_field_binding (function
    | bindings, [] ->
        cont bindings
    | _, who :: _ ->
        err who "expected end of fields" )

and parse_exprs : expr list many_parser =
 fun sexps err cont ->
  parse_many_with sexps parse_expr (function
    | exprs, [] ->
        cont exprs
    | _, who :: _ ->
        err who "expected end of body" )

(* a body is a list of declarations, then a list of expressions *)
and parse_body : body many_parser =
 fun sexps err cont ->
  parse_many_with sexps parse_decl (fun (decls, rest) ->
      Logs.debug (fun m ->
          m "Parsed decls: %a@\nRemaining %a"
            (Format.pp_print_list pp_decl)
            decls
            (Format.pp_print_list Sexp.pp_hum)
            rest ) ;
      parse_exprs rest err (fun exprs -> cont (decls, BeginE exprs)) )

let report_sexp_mismatch (sexp : Sexp.t) (expected : string) =
  failwith
    (Printf.sprintf "Error parsing %s: %s" (Sexp.to_string_hum sexp) expected)

let run sexp : program =
  let open Sexp in
  match sexp with
  | List [Atom "toplevel"; List sexps] ->
      parse_body sexps report_sexp_mismatch (fun x -> x)
  | who ->
      report_sexp_mismatch who "expected `toplevel` program"

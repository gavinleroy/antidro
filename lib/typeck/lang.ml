open Sexplib.Std
module Sexp = Sexplib.Sexp
open Ant.Ty

type fexpr = Resolved of Symbol.t | Anon of expr [@@deriving sexp, show]

and expr =
  | VoidE
  | TrueE
  | FalseE
  | LitE of int
  | StringE of string
  | PlaceE of Place.t
  | IfE of expr * expr * expr
  | LetRecE of Symbol.t * Ty.t * expr * expr
  | LetE of Symbol.t * Ty.t * expr * expr
  | RefE of expr
  | StructE of (Slot.t * Ty.t * expr) list
  | SetE of Place.t * expr
  | FnE of Signature.t * expr
  | AppE of {fty: Ty.t; tyargs: Ty.t list; f: fexpr; args: expr list}
[@@deriving sexp, show]

type program = expr [@@deriving sexp, show]

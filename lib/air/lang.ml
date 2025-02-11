open Sexplib.Std
open Ant.Ty
module AntStd = Ant.Std.WithDom

type formals = (Symbol.t * Ty.t) list [@@deriving sexp_of, show]

and stmt =
  | LetS of (Symbol.t * expr)
  | IfS of (expr * body * body)
  | ReturnS of expr
  | JSBody of string
[@@deriving sexp_of, show]

and expr =
  | VoidE
  | TrueE
  | FalseE
  | LitE of int
  | StringE of string
  | PlaceE of Place.t
  | PrimE of AntStd.Prim.t
  (* | IfE of expr * expr * expr *)
  | StructE of (Slot.t * expr) list
  | SetE of Place.t * expr
  | FnE of formals * body
  | AppE of (expr * expr list)
[@@deriving sexp_of, show]

and body = stmt list [@@deriving sexp_of, show]

and program = body [@@deriving sexp_of, show]

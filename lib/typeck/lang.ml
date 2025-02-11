open Sexplib.Std
open Ant.Ty
module AntStd = Ant.Std.WithDom

type formals = (Symbol.t * Ty.t) list [@@deriving sexp_of, show]

and expr =
  | VoidE
  | TrueE
  | FalseE
  | LitE of int
  | StringE of string
  | PlaceE of Place.t
  | IfE of expr * expr * expr
  | LabelE of
      { groupid: Symbol.t
      ; overloadid: Symbol.t
      ; args: formals
      ; fbody: expr
      ; body: expr }
  | LetE of {bound: Symbol.t; ty: Ty.t; expr: expr; body: expr}
  | RefE of Symbol.t
  | DerefE of Symbol.t
  | StructE of (Slot.t * Symbol.t) list
  | SetE of Place.t * Symbol.t
  | FnE of formals * expr
  | AppE of {f: Place.t; args: Symbol.t list}
[@@deriving sexp_of, show]

type program = expr [@@deriving sexp_of, show]

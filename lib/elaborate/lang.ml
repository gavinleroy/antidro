open Sexplib.Std

module CarryOver = struct
  module Identifier = Typeck.Identifier
  module Place = Typeck.Place
  module Slot = Typeck.Slot
  module Signature = Typeck.Signature
  module Rho = Typeck.Rho
  module IdMap = Typeck.IdMap

  type ty = Typeck.ty [@@deriving sexp, show]

  type eff = Typeck.eff [@@deriving sexp, show]

  type polysig = Typeck.polysig [@@deriving sexp, show]
end

include CarryOver

type expr =
  | VoidE
  | TrueE
  | FalseE
  | LitE of int
  | StringE of string
  | PlaceE of Place.t
  | LetE of Identifier.t * ty * expr * expr
  | StructE of (Slot.t * Identifier.t) list
  | NewE of Identifier.t * Identifier.t list
  | FnE of polysig * expr
  | AppE of Identifier.t * Identifier.t list
  | InlineJS of string
  (* we introduce new forms for lambdas because Fn/App will get lifted *)
  (* into objects, while Thunk/Force will remain inline closures *)
  | LamE of Identifier.t list * expr
  | SetE of Place.t * Identifier.t
[@@deriving sexp, show]

and program = {expr: expr; ty: ty}

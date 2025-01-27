open Sexplib.Std
module Sexp = Sexplib.Sexp
module Prev = Elaborate

module CarryOver = struct
  module Identifier = Prev.Identifier
  module Place = Prev.Place
  module Slot = Prev.Slot
  module Signature = Prev.Signature

  type polysig = Prev.polysig [@@deriving sexp, show]
end

include CarryOver

type stmt =
  | LetS of Identifier.t * expr
  | SetS of Place.t * Identifier.t
  | ReturnS of expr

and body = stmt list [@@deriving sexp, show]

and expr =
  | VoidE
  | TrueE
  | FalseE
  | LitE of int
  | StringE of string
  | InlineJS of string
  | PlaceE of Place.t
  | StructE of (Slot.t * Identifier.t) list
  | NewE of Identifier.t * Identifier.t list
  | LamE of Identifier.t list * body
  | AppE of Identifier.t * Identifier.t list
[@@deriving sexp, show]

and class_ = {name: Identifier.t; psig: polysig; body: body}
[@@deriving sexp, show]

and program = {classes: class_ list; entry: Identifier.t option}
[@@deriving sexp, show]

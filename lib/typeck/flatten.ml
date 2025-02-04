[@@@warning "-a"]

open Env
open Lang
open Ant.Ty
open Ant.Util
open ResultMonad

let flatten_expr ectx = function
  | VoidE
  | TrueE
  | FalseE
  | LitE _i
  | StringE _s
  | PlaceE _pl
  | IfE (_test, _conseq, _alt)
  | LetRecE (_id, _ty, _expr, _body)
  | LetE {bound= _; ty= _; deps= _; expr= _; body= _}
  | RefE _e
  | StructE _fields
  | SetE (_pl, _expr)
  | FnE (_sg, _body)
  | AppE {fty= _; tyargs= _; f= _; args= _} ->
      failwith "NYI"

let run (p : program) : program = flatten_expr p

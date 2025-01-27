open Ty

let types = []

let math =
  let x = Symbol.var "x" and y = Symbol.var "y" in
  [ `Method
      ( "+"
      , Symbol.var "pluspr__"
      , Ty.arrow
          ~deps:(DepsT [(Place.result, [Place.baseid x; Place.baseid y])])
          ~effs:(Ty.effects [])
          (List.combine [x; y] [Ty.number; Ty.number])
          Ty.number )
  ; `Method
      ( "print"
      , Symbol.var "printpr__"
      , Ty.arrow
          ~deps:(Ty.dependencies [])
          ~effs:(Ty.effects [])
          (List.combine [x] [Ty.number])
          Ty.void )
  ] [@@ocamlformat "disable"]

let pervasives = math

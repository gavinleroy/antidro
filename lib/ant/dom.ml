open Ty

let node_id = TyVar.var "node"

let node = VarT node_id

let types = [
  `Type
    (node_id
    , TyFnC ([], Ty.struct_ [(Slot.children, node)]))
] [@@ocamlformat "disable"]

let components =
  let nth_child n pl = Dep.on (Place.result |> Place.slot Slot.children |> Place.offset n) [pl]
  and div_id = Symbol.var "divpr__"
  and x = Symbol.var "x"
  and y = Symbol.var "y" in
  [ `Method
      ( "div"
      , div_id
      (* ([x Node] [y Node]) -> Node {result.children[0]^[x], result.children[1]^[y]}#{}*)
      , Ty.arrow
          ~deps:(Ty.dependencies (Dependencies.of_list [nth_child 0 (Place.baseid x); nth_child 1 (Place.baseid y)]))
          ~effs:(Ty.effects [])
          (List.combine [x; y] [node; node])
          node
      )
  ; `Method
      ( "div"
      , div_id
      (* ([x Number]) -> Node {result.children[0]^[x]}#{} *)
      , Ty.arrow
          ~deps:(Ty.dependencies (Dependencies.of_list [nth_child 0 (Place.baseid x)]))
          ~effs:(Ty.effects [])
          (List.combine [x] [Ty.number])
          node )
  ; `Method
      ( "button"
      , Symbol.var "buttonpr__"
      , let effty = TyVar.fresh () and depty = TyVar.fresh () in
        (* forall<R,E> ([x String], [y (() -> Void R#E)]) -> Node {result.children[0]^[x]}#{}*)
        Ty.arrow
          ~tyvars:[depty; effty]
          ~deps:(Ty.dependencies (Dependencies.of_list [nth_child 0 (Place.baseid x)]))
          ~effs:(Ty.effects [])
          (List.combine
             [x; y]
             [Ty.string; Ty.func ~deps:(VarT depty) ~effs:(VarT effty) [] Ty.void])
          node )
  ; `Method
      ( "render-into"
      , Symbol.var "render_intopr__"
      (* (String, Node) -> Void {}#{} *)
      , Ty.arrow
          ~deps:(Ty.dependencies []) ~effs:(Ty.effects [])
          (List.combine [x; y] [Ty.string; node]) Ty.void )

  ] [@@ocamlformat "disable"]

let pervasives = components

open Ty

type overload = Symbol.t * Ty.t * string

type primitive = [`Method of string * Symbol.t * overload list]

let nth_child' : int -> Place.t =
 fun n -> Place.result |> Place.slot Slot.children |> Place.offset n

module Base = struct
  let types = []

  let math : primitive list =
    let x = Symbol.var "x" and y = Symbol.var "y" in
    [ `Method
        ( "+"
        , Symbol.var "pluspr__"
        , [ Symbol.var "pluspr_n_n__"
          , Ty.arrow
              ~deps:(DepsT (Dependencies.of_list [Dep.on Place.result [Place.baseid x; Place.baseid y]]))
              ~effs:(Ty.effects Effects.empty)
              (List.combine [x; y] [Ty.number; Ty.number])
              Ty.number
          , Format.asprintf
              "return { %a: %a + %a };"
              Slot.pp Slot.return
              Symbol.pp x
              Symbol.pp y
          ]
        )
    ; `Method
        ( "print"
        , Symbol.var "printpr__"
        , [ Symbol.var "printpr_n__"
          , Ty.arrow
              ~deps:(Ty.dependencies Dependencies.empty)
              ~effs:(Ty.effects Effects.empty)
              (List.combine [x] [Ty.number])
              Ty.void
          , Format.asprintf
              "return { %a: console.log(%a) };"
              Slot.pp Slot.return
              Symbol.pp x
          ]
        )
    ] [@@ocamlformat "disable"]

  let pervasives : primitive list = math
end

module WithDom = struct
  let node_id = TyVar.var "node"

  let node = VarT node_id

  let types = Base.types @ [
      `Type
        (node_id
        , TyFnC ([], Ty.struct_ [(Slot.children, node)]))
    ] [@@ocamlformat "disable"]

  let components =
    let nth_child n pl = Dep.on (Place.result |> Place.slot Slot.children |> Place.offset n) [pl]
    and x = Symbol.var "x"
    and y = Symbol.var "y" in
    [ `Method
        ( "div"
        , Symbol.var "divpr__"
        , [ Symbol.var "divpr_el_el__"
          (* ([x Node] [y Node]) -> Node {result.children[0]^[x], result.children[1]^[y]}#{}*)
          , Ty.arrow
              ~deps:(Ty.dependencies (Dependencies.of_list [nth_child 0 (Place.baseid x); nth_child 1 (Place.baseid y)]))
              ~effs:(Ty.effects Effects.empty)
              (List.combine [x; y] [node; node])
              node
          , Format.asprintf
              "let d = document.createElement('div'); \
               d.appendChild(%a); \
               d.appendChild(%a); \
               return { \
               %a: d, \
               %a: (e) => { d.replaceChild(e, d.childNodes[0]); }, \
               %a: (e) => { d.replaceChild(e, d.childNodes[1]); } \
               }; \
              "
              Symbol.pp x
              Symbol.pp y
              Slot.pp Slot.return
              Slot.pp (Place.to_updater_slot (nth_child' 0))
              Slot.pp (Place.to_updater_slot (nth_child' 1))
          ; Symbol.var "divpr_n__"
          , Ty.arrow
              ~deps:(Ty.dependencies (Dependencies.of_list [nth_child 0 (Place.baseid x)]))
              ~effs:(Ty.effects Effects.empty)
              (List.combine [x] [Ty.number])
              node
          , Format.asprintf
              "let d = document.createElement('div'); \
               d.innerHTML = %a; \
               return { \
               %a: d, \
               %a: (n) => { d.innerHTML = n; } \
               }; \
              "
              Symbol.pp x
              Slot.pp Slot.return
              Slot.pp (Place.to_updater_slot (nth_child' 0))
          ]
        )
    ; `Method
        ( "button"
        , Symbol.var "buttonpr__"
        , [ Symbol.var "buttonpr_s_cb__"
          , (let effty = TyVar.fresh () and depty = TyVar.fresh () in
             (* forall<R,E> ([x String], [y (() -> Void R#E)]) -> Node {result.children[0]^[x]}#{}*)
             Ty.arrow
               ~tyvars:[depty; effty]
               ~deps:(Ty.dependencies (Dependencies.of_list [nth_child 0 (Place.baseid x)]))
               ~effs:(Ty.effects Effects.empty)
               (List.combine
                  [x; y]
                  [Ty.string; Ty.func ~deps:(VarT depty) ~effs:(VarT effty) [] Ty.void])
               node)
          , Format.asprintf
              "let b = document.createElement('button');
             b.appendChild(document.createTextNode(%a));
             b.onclick = %a;
             return { \
               %a: b, \
               %a: (s) => { \
               b.replaceChild( \
               document.createTextNode(s), \
               b.childNodes[0])
                   } \
               }; \
              "
              Symbol.pp x
              Symbol.pp y
              Slot.pp Slot.return
              Slot.pp (Place.to_updater_slot (nth_child' 0))
          ]
        )
    ; `Method
        ( "render-into"
        , Symbol.var "render_intopr__"
        , [ Symbol.var "render_intopr_el__"
          (* (String, Node) -> Void {}#{} *)
          , Ty.arrow
              ~deps:(Ty.dependencies Dependencies.empty)
              ~effs:(Ty.effects Effects.empty)
              (List.combine [x; y] [Ty.string; node]) Ty.void
          , Format.asprintf
              "window.onload = () => { \
               let root = document.getElementById(%a); \
               root.appendChild(%a); \
               }; \
               return { %a: void(0) }; \
              "
              Symbol.pp x
              Symbol.pp y
              Slot.pp Slot.return
          ]
        )

    ] [@@ocamlformat "disable"]

  let pervasives : primitive list = Base.pervasives @ components
end

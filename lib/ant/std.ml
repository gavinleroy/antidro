open Ty

module Shared = struct
  type func_data = {name: string; id: Symbol.t; ty: Ty.t; impl: string}

  type overload_data = {id: Symbol.t; ty: Ty.t; impl: string}

  type method_data =
    {name: string; groupid: Symbol.t; overloads: overload_data list}

  type primitive = MethodPrim of method_data | FuncPrim of func_data
end

let nth_child' : int -> Place.t =
 fun n -> Place.return |> Place.slot Slot.children |> Place.offset n

let no_effs = Effects.to_ty Effects.empty

let no_deps = Dependencies.to_ty Dependencies.empty

module Base = struct
  include Shared

  module Prim = struct
    type t = ..

    let show = function _ -> "#<prim>"

    let sexp_of_t t = Sexp.Atom (show t)

    let pp ppf t = Format.fprintf ppf "%s" (show t)
  end

  (* Built-In Definitions *)

  let to_string_n = Symbol.var "to_string_n__"

  let plus_n_n = Symbol.var "plus_n_n__"

  let fill_array = Symbol.var "fill_array__"

  let map = Symbol.var "map_array__"

  let print_n = Symbol.var "print_n__"

  let print_s = Symbol.var "print_s__"

  let defined =
    [ to_string_n
    ; plus_n_n
    ; fill_array
    ; print_n
    ] [@@ocamlformat "disable"]

  (* ******************** *)

  let types = []

  let niceties : primitive list =
    let x = Symbol.var "x" in
    [ MethodPrim
      { name= "to-string"
      ; groupid= Symbol.var "to_stringpr__"
      ; overloads= [ { id= to_string_n
                     ; ty= Ty.arrow
                             ~effs:no_effs
                             ~deps:(Dependencies.to_ty (Dependencies.of_list [Dep.on Place.return [Place.baseid x]]))
                             [(x, Ty.number)]
                             Ty.string
                     ; impl= Format.asprintf
                               "return { %a: { value: %a.toString() } };"
                               Slot.pp Slot.return
                               Symbol.pp x
                     } ]}
    ] [@@ocamlformat "disable"]

  let math : primitive list =
    let x = Symbol.var "x" and y = Symbol.var "y" in
    [ MethodPrim
        { name= "+"
        ; groupid= Symbol.var "pluspr__"
        ; overloads= [ { id= plus_n_n
                       ; ty= Ty.arrow
                             ~deps:(Dependencies.to_ty (Dependencies.of_list [Dep.on Place.return [Place.baseid x; Place.baseid y]]))
                             ~effs:no_effs
                             [ x, Ty.number
                             ; y, Ty.number
                             ]
                             Ty.number
                       ; impl= Format.asprintf
                            (* FIXME *)
                             "return { %a: { value: %a.value + %a.value } };"
                             Slot.pp Slot.return
                             Symbol.pp x
                             Symbol.pp y }
                     ]
        }
    ] [@@ocamlformat "disable"]

  let array : primitive list =
    let x = Symbol.var "x" and y = Symbol.var "y" and f = Symbol.var "f" in
    [ FuncPrim
        { name= "fill-array"
        ; id= fill_array
        (* forall<T, R> *)
        (*  (f: (n: Number, (i: Number) -> T | result^R)) -> Array<T> | result^n, result[*]^R *)
        ; ty= (let t = TyVar.fresh () and r = TyVar.fresh () and e = TyVar.fresh () in
               Ty.arrow
                 ~tyvars:[r; e; t]
                 ~effs:no_effs
                 (* result^{x,f} *)
                 (* TODO: result[*]^r <-- what is `r` exactly, it has a different scope than the inner `r` *)
                 ~deps:no_deps (* FIXME *)
                 (List.combine
                    [f; x]
                    [ Ty.func
                        ~deps:(VarT r)
                        ~effs:(VarT e)
                        [(y, Ty.number)]
                        (VarT t)
                    ; Ty.number ] )
                 (Ty.array (VarT t)))
        ; impl= Format.asprintf
              "let arr = []; \
               autorun(() => { \
               arr = []; \
               for (let i = 0; i < %a.value; i++) { \
               let el = %a(i).%a; \
               arr.push(el); \
               }}); \
               return { %a: arr }; \
              "
              Symbol.pp x
              Symbol.pp f
              Slot.pp Slot.return
              Slot.pp Slot.return
        }
      ; FuncPrim
        { name= "map"
        ; id= map
        ; ty= (let t = TyVar.fresh () and u = TyVar.fresh () and r = TyVar.fresh () in
               Ty.arrow
                 ~tyvars:[t; u; r]
                 ~effs:no_effs
                 ~deps:no_deps (* FIXME *)
                 (List.combine
                    [ f; x ]
                    [ Ty.func
                        ~deps:(VarT r)
                        ~effs:no_effs
                        [(y, VarT t)]
                        (VarT u)
                    ; Ty.array (VarT t)] )
                 (Ty.array (VarT u)) )
        ; impl= Format.asprintf
            " \
            const mappedArray = observable([], { deep: false }); \
            reaction( \
              () => ({ \
                items: %a.slice(), \
                length: %a.length, \
              }), \
              ({ items }) => { \
                mappedArray.replace(items.map((item, index) => %a(item, index).%a)); \
              }, \
              { fireImmediately: true } \
            ); \
            return { %a: mappedArray }; \
            "
            Symbol.pp x
            Symbol.pp x
            Symbol.pp f
            Slot.pp Slot.return
            Slot.pp Slot.return
        }

    ] [@@ocamlformat "disable"]

  let io : primitive list =
  let x = Symbol.var "x" in
  [ MethodPrim
      { name= "print"
      ; groupid= Symbol.var "printpr__"
      ; overloads= [ { id= print_n
                     ; ty= Ty.arrow
                           ~effs:no_effs
                           ~deps:no_deps
                           [x, Ty.number]
                           Ty.void
                     ; impl= Format.asprintf
                           "return { %a: console.log(%a) };"
                           Slot.pp Slot.return
                           Symbol.pp x }
                   ; { id= print_s
                     ; ty= Ty.arrow
                           ~effs:no_effs
                           ~deps:no_deps
                           [ x, Ty.string
                           ]
                           Ty.void
                     ; impl= Format.asprintf
                           "return { %a: console.log(%a) };"
                           Slot.pp Slot.return
                           Symbol.pp x }
                   ]
      }
  ] [@@ocamlformat "disable"]

  let pervasives : primitive list = niceties @ math @ array @ io
end

module WithDom = struct
  include Shared

  module Prim = struct
    include Base.Prim

    type t += ObservableP

    let show = function ObservableP -> "observable" | p -> show p

    let sexp_of_t t = Sexp.Atom (show t)

    let pp ppf t = Format.fprintf ppf "%s" (show t)
  end

  let node_id = TyVar.var "node"

  let node = VarT node_id

  let wrap_value_slot = Slot.of_string "value"

  (* ****************** *)
  (* Built-in Functions *)
  (* ****************** *)

  let div_el_el = Symbol.var "div_el_el__"

  let div_el_els = Symbol.var "div_el_els__"

  let div_n = Symbol.var "div_n__"

  let div_s = Symbol.var "div_s__"

  let button_s_f = Symbol.var "buttonpr_s_cb__"

  let render_el = Symbol.var "renderpr_el__"

  let wrap = Symbol.var "wrap__"

  let defined : Symbol.t list = Base.defined @
    [ div_el_el
    ; div_el_els
    ; div_n
    ; div_s
    ; button_s_f
    ; render_el
    ; wrap
    ] [@@ocamlformat "disable"]

  (* ****************** *)

  let types = Base.types @ [
      `Type
        (node_id
        , TyFnC ([], Ty.struct_ [(Slot.children, node)]))
    ] [@@ocamlformat "disable"]

  let components =
    let nth_child n pl = Dep.on (Place.return |> Place.slot Slot.children |> Place.offset n) [pl]
    and x = Symbol.var "x"
    and y = Symbol.var "y" in
    [ MethodPrim
        { name= "div"
        ; groupid= Symbol.var "divpr__"
        ; overloads= [ { id= div_el_el
                       (* ([x Node] [y Node]) -> Node {result.children[0]^[x], result.children[1]^[y]}#{}*)
                       ; ty= Ty.arrow
                             ~effs:no_effs
                             ~deps:(Dependencies.to_ty (Dependencies.of_list [nth_child 0 (Place.baseid x); nth_child 1 (Place.baseid y)]))
                             [x, node; y, node]
                             node
                       ; impl= Format.asprintf
                             "let d = document.createElement('div'); \
                              autorun(() => { \
                                d.replaceChildren(%a, %a); \
                              });\
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
                             Slot.pp (Place.to_updater_slot (nth_child' 1)) }
                     ; { id= div_el_els
                       (* ([x Node] [y (Array Node)]) -> Node {result.children[0]^[x], result.children[1]^[y]}#{}*)
                       ; ty= Ty.arrow
                             ~effs:no_effs
                             ~deps:no_deps (* FIXME *)
                             [x, node; y, Ty.array node]
                             node
                       ; impl= Format.asprintf
                             "let d = document.createElement('div'); \
                              autorun(() => { \
                                d.replaceChildren(%a, ...%a); \
                              });\
                              return { \
                              %a: d, \
                              }; \
                             "
                             Symbol.pp x
                             Symbol.pp y
                             Slot.pp Slot.return
                       }
                     ; { id= div_n
                       ; ty= Ty.arrow
                             ~effs:no_effs
                             ~deps:(Dependencies.to_ty (Dependencies.of_list [nth_child 0 (Place.baseid x)]))
                             [x, Ty.number]
                             node
                       ; impl= Format.asprintf
                             "let d = document.createElement('div'); \
                              autorun(() => (d.innerHTML = %a.value)); \
                              return { \
                              %a: d, \
                              %a: (n) => { d.innerHTML = n.value; } \
                              }; \
                             "
                             Symbol.pp x
                             Slot.pp Slot.return
                             Slot.pp (Place.to_updater_slot (nth_child' 0)) }
                     ; { id= div_s
                       ; ty= Ty.arrow
                             ~effs:no_effs
                             ~deps:(Dependencies.to_ty (Dependencies.of_list [nth_child 0 (Place.baseid x)]))
                             [x, Ty.string]
                             node
                       ; impl= Format.asprintf
                             "let d = document.createElement('div'); \
                              autorun(() => (d.innerHTML = %a.value)); \
                              return { \
                              %a: d, \
                              %a: (n) => { d.innerHTML = n.value; } \
                              }; \
                             "
                             Symbol.pp x
                             Slot.pp Slot.return
                             Slot.pp (Place.to_updater_slot (nth_child' 0)) }
                     ]
        }
    ; MethodPrim
        { name= "button"
        ; groupid= Symbol.var "buttonpr__"
        ; overloads= [ { id= button_s_f
                       ; ty= (let r = TyVar.fresh () and e = TyVar.fresh () in
                              (* forall<R,E> ([x String], [y (() -> Void R#E)]) -> Node {result.children[0]^[x]}#{}*)
                              Ty.arrow
                                ~tyvars:[r; e]
                                ~effs:no_effs
                                ~deps:(Dependencies.to_ty (Dependencies.of_list [nth_child 0 (Place.baseid x)]))
                                [ x, Ty.string
                                ; y, Ty.func ~deps:(VarT r) ~effs:(VarT e) [] Ty.void
                                ]
                                node)
                       ; impl= Format.asprintf
                             "let b = document.createElement('button'); \
                              autorun(() => (b.innerHTML = %a.value, b.onclick = %a)); \
                              return { \
                              %a: b, \
                              %a: (s) => { \
                              b.innerHTML = s; \
                              } \
                              }; \
                             "
                             Symbol.pp x
                             Symbol.pp y
                             Slot.pp Slot.return
                             Slot.pp (Place.to_updater_slot (nth_child' 0)) }
                     ]
        }
    ; MethodPrim
        { name= "render"
        ; groupid= Symbol.var "renderpr__"
        ; overloads= [ { id= render_el
                       (* (String, Node) -> Void {}#{} *)
                       ; ty= Ty.arrow
                             ~effs:no_effs
                             ~deps:no_deps
                             [ x, Ty.string
                             ; y, node
                             ]
                             Ty.void
                       ; impl= Format.asprintf
                             " \
                             document.body.appendChild(%a); \
                             return { %a: void(0) }; \
                             "
                             Symbol.pp y
                             Slot.pp Slot.return }
                     ]
        }
    ; FuncPrim
        (
         { name= "wrap"
         ; id= wrap
         ; ty= (let t = TyVar.fresh () in
                (* forall<T> ([t T]) -> { value: T } *)
                (* result.value^{t} *)
                Ty.arrow
                  ~tyvars:[t]
                  ~effs:no_effs
                  ~deps:(Dependencies.to_ty (Dependencies.of_list [Dep.on (Place.slot wrap_value_slot Place.return) [Place.baseid x]]))
                  [x, VarT t]
                  (Ty.struct_ [(wrap_value_slot, VarT t)]))
         ; impl= Format.asprintf
               " \
                return { %a: { %a: %a } }; \
               "
               Slot.pp Slot.return
               Slot.pp wrap_value_slot
               Symbol.pp x
         })
    ] [@@ocamlformat "disable"]

  let pervasives : primitive list = Base.pervasives @ components
end

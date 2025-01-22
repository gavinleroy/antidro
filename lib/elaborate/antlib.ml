open Typeck

let nth_child n =
  Place.func_result |> Place.offset n |> Place.slot (Slot.of_string "children")

let pervasives =
  [ ( Math.plus
    , Signature.basic' [("x", NumberT); ("y", NumberT)] NumberT
    , Format.asprintf "{ %a: x + y }"
      Slot.pp Slot.return
      |> fun inner -> Lang.InlineJS inner )
  ; ( Dom.div_e_e
    , Signature.basic' [("e1", Dom.nodety); ("e2", Dom.nodety)] Dom.nodety
    , Format.asprintf
        "(() => { \
          let d = document.createElement('div'); \
          d.appendChild(e1); \
          d.appendChild(e2); \
          return { \
            %a: d, \
            %a: (e) => { d.replaceChild(e, d.childNodes[0]); }, \
            %a: (e) => { d.replaceChild(e, d.childNodes[1]); } \
          }; \
        })()"
        Slot.pp Slot.return
        Slot.pp (Place.to_updater_slot (nth_child 0))
        Slot.pp (Place.to_updater_slot (nth_child 1))
        |> fun inner -> Lang.InlineJS inner
  )

  ; ( Dom.div_n
    , Signature.basic' [("n", NumberT)] Dom.nodety
    , Format.asprintf
        "(() => { \
          let d = document.createElement('div'); \
          d.innerHTML = n; \
          return { \
            %a: d, \
            %a: (n) => { d.innerHTML = n; } \
          }; \
        })()"
        Slot.pp Slot.return
        Slot.pp (Place.to_updater_slot (nth_child 0))
        |> fun inner -> Lang.InlineJS inner
    )
  ; ( Dom.button_s_onclick
    , (Signature.basic' [("s", StringT); ("onclick", FnT (Signature.polyall [] VoidT))]
         Dom.nodety )
    , Format.asprintf
        "(() => { \
          let b = document.createElement('button');
          b.appendChild(document.createTextNode(s));
          b.onclick = onclick;
          return { \
            %a: b, \
            %a: (s) => { \
              b.replaceChild( \
              document.createTextNode(s), \
              b.childNodes[0])
            } \
          }; \
        })()"
        Slot.pp Slot.return
        Slot.pp (Place.to_updater_slot (nth_child 0))
        |> fun inner -> Lang.InlineJS inner )
    ; ( Dom.render_into
    , Signature.basic'
            [("id", StringT); ("component", Dom.nodety)]
            VoidT
    , Format.asprintf
        "(() => { \
          window.onload = () => {\
            let root = document.getElementById(id);
            root.appendChild(component);
          }; \
          return { %a: void(0) }
        })()"
        Slot.pp Slot.return
        |> fun inner -> Lang.InlineJS inner )
    ][@@ocamlformat "disable"]

let wrap (p : Lang.program) : Lang.program =
  let e = p.expr in
  let wrapped =
    List.fold_right
      (fun (instanceid, psig, body) acc ->
        let classid = Identifier.fresh () in
        (* TODO: change me to upvars instead of freevars *)
        let upvars = Signature.freevars psig in
        Lang.LetE
          ( classid
          , UncallableT
          , FnE (psig, body)
          , LetE (instanceid, FnT psig, NewE (classid, upvars), acc) ) )
      pervasives e
  in
  {p with expr= wrapped}

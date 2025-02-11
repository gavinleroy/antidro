

let rec texpr : expr -> expr = function
  | LitE i
  | StringE s
  | IfE (c, t, f)
  | LabelE
      { groupid
      ; overloadid
      ; args
      ; fbody
      ; body }
  | LetE
      {bound; ty; deps; expr; body}
  | FnE (formals, expr)
  | e -> e

let run (p : program) : program =

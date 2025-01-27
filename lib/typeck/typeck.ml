let sexp_of_program = Lang.sexp_of_program

let run (prog : Parse.program) : Lang.program = Infer.run prog
(* |> Typeit.run *)
(* TODO: add lowering steps here and type each one in-between *)

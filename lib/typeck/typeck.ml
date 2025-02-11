module Lang = Lang

let sexp_of_program = Lang.sexp_of_program

let run (prog : Parse.program) : Lang.program =
  prog |> Simplify.run |> Typeit.run

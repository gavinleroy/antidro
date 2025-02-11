module Lang = Lang

let sexp_of_program = Lang.sexp_of_program

let run (p : Typeck.Lang.program) : Lang.program = Ir.run p |> Wrap.run

open Sexplib

let read_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

let run filename =
  Sexp.load_sexps filename
  |> fun sexps -> Sexp.List [Sexp.Atom "toplevel"; Sexp.List sexps]

let sexp_of_program p = p

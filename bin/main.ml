open Cmdliner

let make_flag ?(doc = "Documentation maybe?") name =
  Arg.(value & flag & info [name] ~doc)

let read = make_flag "read"

let parse = make_flag "parse"

let typeck = make_flag "typeck"

let air = make_flag "air"

let dump = make_flag ~doc:"Dump pass output to stderr" "dump"

let is_ant_file file =
  if Filename.check_suffix file ".ant" then Ok file
  else Error (`Msg ("Not a `.ant` file:" ^ file))

let input_str =
  let doc = "Input string to process" in
  Arg.(
    required
    & pos 0 (some (conv (is_ant_file, Format.pp_print_string))) None
    & info [] ~docv:"INPUT" ~doc )

let output_file file =
  let b = Filename.basename file |> Filename.chop_extension in
  b ^ ".js"

let operation read parse typeck air dump input =
  let outfile = output_file input in
  let stop =
    if read then `Read
    else if parse then `Parse
    else if typeck then `Typeck
    else if air then `Air
    else `Asm
  in
  Antidro.compile ~dump ~stop ~outfile input

let operation_t =
  Term.(const operation $ read $ parse $ typeck $ air $ dump $ input_str)

let info =
  let doc = "Antidro: a concept" in
  Cmd.info "antidro" ~version:"1.0.0" ~doc

let cmd : unit Cmd.t = Cmd.v info operation_t

let () = exit (Cmd.eval cmd)

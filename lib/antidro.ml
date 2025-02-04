let log_level =
  Sys.getenv_opt "ANTIDRO_LOG"
  |> Option.map (fun s ->
    String.lowercase_ascii s
    |> function
    | "debug" -> Logs.Debug
    | "info" -> Logs.Info
    | "warn" -> Logs.Warning
    | _ -> Logs.Error)
  [@@ocamlformat "disable"]

let stamp_tag : Mtime.span Logs.Tag.def =
  Logs.Tag.def "stamp" ~doc:"Relative monotonic time stamp" Mtime.Span.pp

let stamp c = Logs.Tag.(empty |> add stamp_tag (Mtime_clock.count c))

let reporter ppf =
  let report _src level ~over k msgf =
    let k _ = over () ; k () in
    let with_stamp h _tags k ppf fmt =
      Format.kfprintf k ppf ("%a @[" ^^ fmt ^^ "@]@.") Logs.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k ppf fmt
  in
  {Logs.report}

let log_reporter =
  Sys.getenv_opt "ANTIDRO_LOG"
  |> Option.value ~default:"null"
  |> String.lowercase_ascii
  |> function
  | "null" -> Format.formatter_of_out_channel (open_out Filename.null)
  | _ -> Format.err_formatter
  [@@ocamlformat "disable"]

let () =
  Logs.set_reporter (reporter log_reporter) ;
  Logs.set_level log_level

let compile ?(dump = false) ?(stop = `ASM) ~outfile file =
  ignore outfile ;
  let stopif variant prog =
    if variant = stop then exit 0 ;
    prog
  in
  let dumpit stage f x =
    let chnl = stderr in
    if dump then (
      output_string chnl "=== " ;
      output_string chnl stage ;
      output_string chnl " ===\n" ;
      Sexplib.Sexp.output_hum chnl (f x) ;
      output_string chnl " \n\n" ) ;
    x
  in
  Read.run file
  |> dumpit "READ" Read.sexp_of_program
  |> stopif `Read |> Parse.run
  |> dumpit "PARSE" Parse.sexp_of_program
  |> stopif `Parse |> Typeck.run
  |> dumpit "TYPECK" Typeck.sexp_of_program
  |> stopif `Typeck
  |> Emit.run ~out:(open_out outfile)
(* |> Elaborate.run *)
(* |> dumpit "ELABORATE" Elaborate.sexp_of_program *)
(* |> stopif `Elaborate |> Ant.run *)
(* |> dumpit "ANT" Ant.sexp_of_program *)
(* |> stopif `Ant *)
(* |> Emit.run ~out:(open_out outfile) *)
(* |> Emit.run ~out:Out_channel.stdout *)

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
  |> stopif `Typeck |> Elaborate.run
  |> dumpit "ELABORATE" Elaborate.sexp_of_program
  |> stopif `Elaborate |> Ant.run
  |> dumpit "ANT" Ant.sexp_of_program
  |> stopif `Ant
  (* |> Emit.run ~out:(open_out outfile) *)
  |> Emit.run ~out:Out_channel.stdout

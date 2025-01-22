let does_parse filename () =
  let _ =
    Antidro.compile ~stop:`Parse
      ~outfile:(Filename.temp_file "parse_test" "test")
      filename
  in
  Alcotest.(check bool) "parse" true true

let parse_suite =
  [ Utils.get_files_with_extension "simple" "ant"
  ; Utils.get_files_with_extension "valid" "ant" ]
  |> List.flatten
  |> List.map (fun fname ->
         let test_name = Printf.sprintf "parse-%s" fname in
         (test_name, `Quick, does_parse fname) )

let () = Alcotest.run "Antidro_parse" [("Parsing", parse_suite)]

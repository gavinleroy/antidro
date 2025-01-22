let does_parse filename () =
  let _ =
    Antidro.compile ~stop:`Typeck
      ~outfile:(Filename.temp_file "typeck_test" "test")
      filename
  in
  Alcotest.(check bool) "typeck" true true

let typeck_suite =
  Utils.get_files_with_extension "simple" "ant"
  |> List.map (fun fname ->
         let test_name = Printf.sprintf "parse-%s" fname in
         (test_name, `Quick, does_parse fname) )

let () = Alcotest.run "Antidro_typeck" [("Typing", typeck_suite)]

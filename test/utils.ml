let get_files_with_extension dir ext =
  let filenames = Sys.readdir dir in
  Array.to_list filenames
  |> List.filter (fun file -> Filename.check_suffix file ext)
  |> List.map (fun file -> Filename.concat dir file)

let read_lines file_name =
  let file = open_in file_name in
  let rec go acc =
    try
      let line = input_line file in
      go (line :: acc)
    with End_of_file ->
      close_in file;
      acc
  in
  List.rev (go [])

let string_of_list inner list =
  "[" ^ String.concat ", " (List.map inner list) ^ "]"

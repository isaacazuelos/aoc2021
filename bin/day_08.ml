open Advent_of_code.Io
open Advent_of_code.Util

let input =
  let lines = read_lines "./inputs/day_08.txt" in
  let pipe line =
    match String.split_on_char '|' line with
    | [ l; r ] -> (l, r)
    | _ -> raise Parsing.Parse_error
  in
  let parse line =
    let signals, display = pipe line in
    (String.split_on_char ' ' signals, String.split_on_char ' ' display)
  in
  List.map parse lines

let candidates_by_length length =
  match length with
  | 2 -> [ 1 ]
  | 3 -> [ 7 ]
  | 4 -> [ 4 ]
  | 5 -> [ 2; 3; 5 ]
  | 6 -> [ 0; 6; 9 ]
  | 7 -> [ 8 ]
  | _ -> []

(* let solve (signals, display) = [] *)

let part_1 () =
  let pred = function 1 | 4 | 7 | 8 -> true | _ -> false in
  let candidates =
    (* Makes you appreciate >>= and do notation. *)
    List.concat_map
      (List.concat_map (candidates_by_length % String.length) % snd)
  in
  count pred (candidates input)

let print_answer () = Printf.printf "day 07 part 1: %d\n" (part_1 ())
(* Printf.printf "day 07 part 2: %d\n" (part_2 ()) *)

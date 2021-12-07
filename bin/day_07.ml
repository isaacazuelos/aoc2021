open Advent_of_code.Io
open Advent_of_code.Util

let input =
  let lines = read_lines "./inputs/day_07.txt" in
  List.map int_of_string (String.split_on_char ',' (List.hd lines))

let list_max = List.fold_left max 0

let list_min = List.fold_left min max_int

let positions = range (list_min input) (list_max input)

let min_fuel cost_calc =
  let cost_to_move_everyone_to position =
    List.fold_left (fun acc start -> cost_calc start position + acc) 0 input
  in
  list_min (List.map cost_to_move_everyone_to positions)

let part_1 () =
  let cost_calc start position = abs (start - position) in
  min_fuel cost_calc

let part_2 () =
  (* it's a triangle with sides of legth abs (start - position) *)
  let cost_for_position start position =
    let side = abs (start - position) in
    side * (side + 1) / 2
  in

  min_fuel cost_for_position

let print_answer () =
  Printf.printf "day 07 part 1: %d\n" (part_1 ());
  Printf.printf "day 07 part 2: %d\n" (part_2 ())

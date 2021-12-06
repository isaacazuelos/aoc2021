open Advent_of_code.Util
open Advent_of_code.Io

let input =
  (* let lines = read_lines "./inputs/day_01_example.txt" in *)
  let lines = read_lines "./inputs/day_01.txt" in
  List.map int_of_string lines

let count_increases depths =
  let changes = zip (List.tl depths) depths in
  count (uncurry ( > )) changes

let part_1 () = count_increases input

let part_2 () =
  let depths = input in
  let windows = window3 depths in
  let window_sums = List.map (fun (x, y, z) -> x + y + z) windows in
  count_increases window_sums

let print_answer () =
  Printf.printf "day 01 part 1: %d\n" (part_1 ());
  Printf.printf "day 01 part 1: %d\n" (part_1 ())

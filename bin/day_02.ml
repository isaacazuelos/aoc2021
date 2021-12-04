(* open Advent_of_code.Util *)
open Advent_of_code.Io

type direction = Forward of int | Down of int | Up of int

let string_of_direction direction =
  match direction with
  | Forward n -> "forward " ^ string_of_int n
  | Down n -> "down " ^ string_of_int n
  | Up n -> "up " ^ string_of_int n

let parse line =
  match Str.split (Str.regexp " ") line with
  | [ "forward"; number ] -> Forward (int_of_string number)
  | [ "down"; number ] -> Down (int_of_string number)
  | [ "up"; number ] -> Up (int_of_string number)
  | _ -> raise Parsing.Parse_error
(* not sure how idomatic reuse is here*)

let input =
  let lines = read_lines "./inputs/day_02.txt" in
  List.map parse lines

let part_1 () =
  let step (forward, depth) direction =
    match direction with
    | Forward n -> (forward + n, depth)
    | Down n -> (forward, depth + n)
    | Up n -> (forward, depth - n)
  in
  let f, d = List.fold_left step (0, 0) input in
  f * d

let part_2 () =
  let step (forward, depth, aim) direction =
    match direction with
    | Down n -> (forward, depth, aim + n)
    | Up n -> (forward, depth, aim - n)
    | Forward n -> (forward + n, depth + (aim * n), aim)
  in
  let f, d, _ = List.fold_left step (0, 0, 0) input in
  f * d

let print_answer () =
  Printf.printf "day 2 part 1: %d\n" (part_1 ());
  Printf.printf "day 2 part 2: %d\n" (part_2 ())

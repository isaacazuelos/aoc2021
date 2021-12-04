open Advent_of_code.Io
open Advent_of_code.Util

let parse line =
  let parse_char acc c =
    match c with
    | '0' -> false :: acc
    | '1' -> true :: acc
    | _ -> raise Parsing.Parse_error
  in
  List.rev (String.fold_left parse_char [] line)

let input =
  let lines = read_lines "./inputs/day_03.txt" in
  List.map parse lines

let counts column =
  let count_one = count id in
  let count_zero = count not in
  (count_zero column, count_one column)

let part_1 () =
  let counts = List.map counts (transpose input) in
  let gamma_bits = List.map (uncurry ( > )) counts in
  let gamma = int_of_bits gamma_bits in
  let epsilon = int_of_bits (List.map not gamma_bits) in
  gamma * epsilon

let filter_process criteria =
  let row_length = List.length (List.nth input 0) in

  (* This is a bit annoying. I had to resort to mutability here because writing
     a type of `List.filter` that can exit early is actually a really annoying
     type of fold. I also had to use `else ()` and just continue for a bunch of
     iterations which may not do anything since OCaml doesn't have a `break`.
     You can emulate them with an exception, but the row lenght isn't long
     enough to bother.
  *)
  let candidates = ref input in
  for bit_position = 0 to row_length - 1 do
    if List.length !candidates > 1 then
      let column = List.map (fun l -> List.nth l bit_position) !candidates in
      let wanted = criteria (counts column) in
      let has_correct_bit row = List.nth row bit_position == wanted in

      candidates := List.filter has_correct_bit !candidates
    else ()
  done;
  int_of_bits (List.hd !candidates)

let part_2 () =
  let oxygen_generator_rating = filter_process (uncurry ( <= )) in
  let co2_scrubber_rating = filter_process (uncurry ( > )) in
  oxygen_generator_rating * co2_scrubber_rating

let print_answer () =
  Printf.printf "day 3 part 1: %d\n" (part_1 ());
  Printf.printf "day 3 part 2: %d\n" (part_2 ())

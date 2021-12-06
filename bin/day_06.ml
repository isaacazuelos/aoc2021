open Advent_of_code.Io

let input =
  let lines = read_lines "./inputs/day_06.txt" in
  let line = List.hd lines in
  List.map int_of_string (String.split_on_char ',' line)

let reproduce age =
  let days_after_spawn = 6 in
  let days_after_hatch = 8 in
  match age with 0 -> [ days_after_spawn; days_after_hatch ] | n -> [ pred n ]

let next_gen = List.concat_map reproduce

let part_1 () =
  let fishes = ref input in
  for _i = 1 to 80 do
    fishes := next_gen !fishes
  done;
  List.length !fishes

(* Okay so that's too inefficient. We can instead of moving the whole list just
   track the number of fish at each age. *)

let counts fish =
  let count = Array.make 9 0 in
  let update_count age = Array.set count age (succ (Array.get count age)) in
  List.iter update_count fish;
  count

(* Wasted a fair bit of time here trying to avoid allocating a new list here for
   basically no reason. In retrospec just like 8 calls to `Array.set` would have
   worked instead of getting fancy, or allocating. *)
let next_gen_counts fish =
  match fish with
  | [| births; a0; a1; a2; a3; a4; a5; a6; a7 |] ->
      [| a0; a1; a2; a3; a4; a5; a6 + births; a7; births |]
  | _ -> raise (Invalid_argument "invalid fish ages")

let part_2 () =
  (* I should really avoid the ref and not allocate in the loop but it's bee
     fast enough. *)
  let fishes = ref (counts input) in
  for _i = 1 to 256 do
    fishes := next_gen_counts !fishes
  done;
  Array.fold_left ( + ) 0 !fishes

let print_answer () =
  Printf.printf "day 06 part 1: %d\n" (part_1 ());
  Printf.printf "day 06 part 2: %d\n" (part_2 ())

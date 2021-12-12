open Advent_of_code.Io
open Advent_of_code.Util

(* https://discuss.ocaml.org/t/how-to-sort-a-string-in-ocaml/4904  *)
let string_sort s =
  let n = String.length s in
  let a = Array.init n (fun i -> s.[i]) in
  Array.sort Char.compare a;
  String.init n (fun i -> a.(i))

(* (signals, display) where signals: [string], display: [string] *)
let input =
  let lines = read_lines "./inputs/day_08.txt" in
  let pipe line =
    match String.split_on_char '|' line with
    | [ l; r ] -> (l, r)
    | _ -> raise Parsing.Parse_error
  in
  let parse line =
    let b, a = pipe line in
    let signals, displays = (words b, words a) in
    let sort2 l = (sort_by_key String.length) (List.map string_sort l) in

    (sort2 signals, List.map string_sort displays)
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

let part_1 () =
  let pred = function 1 | 4 | 7 | 8 -> true | _ -> false in
  let candidates =
    (* Makes you appreciate >>= and do notation. *)
    List.concat_map
      (List.concat_map (candidates_by_length % String.length) % snd)
  in
  count pred (candidates input)

let is_display_subset segments display =
  String.for_all (String.contains display) segments

(* The usual subset with equality, not strict subset. For ordering, think:
   ```
   display `display_subtract` segmeents
   ```
*)
let display_subtract lhs rhs =
  string_sort
    (String.concat ""
       (String.fold_left
          (fun acc c ->
            if String.contains rhs c then acc else String.make 1 c :: acc)
          [] lhs))

let display_eq a b = is_display_subset a b && is_display_subset b a

let tup3_map f (a, b, c) = (f a, f b, f c)

let solve signals =
  let one, seven, four, (f1, f2, f3), (s1, s2, s3), eight =
    match signals with
    | [ s0; s1; s2; s3; s4; s5; s6; s7; s8; s9 ] ->
        (s0, s1, s2, (s3, s4, s5), (s6, s7, s8), s9)
    | _ -> raise (Invalid_argument "got too many signals?")
  in

  let contains_one = is_display_subset one in

  let six, (zero, nine) =
    let six_nine s1 s2 =
      if is_display_subset four s2 then (s1, s2) else (s2, s1)
    in
    match tup3_map (not % contains_one) (s1, s2, s3) with
    | true, false, false -> (s1, six_nine s2 s3)
    | false, true, false -> (s2, six_nine s1 s3)
    | false, false, true -> (s3, six_nine s1 s2)
    | _ -> raise (Invalid_argument "error finding six")
  in

  let three, (two, five) =
    let two_five s1 s2 =
      let e = display_subtract eight nine in
      if is_display_subset e s1 then (s1, s2) else (s2, s1)
    in
    match tup3_map contains_one (f1, f2, f3) with
    | true, false, false -> (f1, two_five f2 f3)
    | false, true, false -> (f2, two_five f1 f3)
    | false, false, true -> (f3, two_five f1 f2)
    | _ -> raise (Invalid_argument "error finding three")
  in

  (* the index of a display digit in this array is the number on the display *)
  [| zero; one; two; three; four; five; six; seven; eight; nine |]

let find_index arr value =
  let found = ref None in
  for i = 0 to 9 do
    if String.equal (Array.get arr i) value then found := Some i else ()
  done;
  !found

let display_value signals display =
  let solution = solve signals in

  let digit_value digit =
    match find_index solution digit with
    | Some n -> n
    | None -> raise @@ Invalid_argument "no digit value"
  in

  let value =
    match List.map digit_value display with
    | [ a; b; c; d ] -> (a * 1000) + (b * 100) + (c * 10) + d
    | _ -> raise @@ Invalid_argument "display must be 4 digits"
  in

  (* Printf.printf "%04d\n" value; *)
  value

let part_2 () =
  List.fold_left
    (fun acc (signals, display) -> acc + display_value signals display)
    0 input

let print_answer () =
  Printf.printf "day 07 part 1: %d\n" (part_1 ());
  Printf.printf "day 07 part 2: %d\n" (part_2 ())

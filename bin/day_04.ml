open Advent_of_code.Io
open Advent_of_code.Util

(* I'm using just `int option list list` for boards, i.e. `[[int?]]` with lists
   of length 5.
*)

let draws, boards =
  let lines = read_lines "./inputs/day_04.txt" in
  let draws =
    let first_line = List.hd lines in
    let numbers = String.split_on_char ',' first_line in
    List.map int_of_string numbers
  in
  let boards =
    let lines =
      let lines_without_draws = List.tl (List.tl lines) in

      let parse_board_line line =
        let numbers = Str.split (Str.regexp " +") line in
        List.map (Option.some % int_of_string) numbers
      in

      List.map parse_board_line lines_without_draws
    in

    let break_on_empty (current, built) line =
      match line with
      | [] -> ([], List.rev current :: built)
      | row -> (row :: current, built)
    in
    let first, rest = List.fold_left break_on_empty ([], []) lines in
    List.rev (first :: rest)
  in
  (draws, boards)

let print_board board =
  let print_board_line line : unit =
    let cells =
      List.map (function None -> "--" | Some n -> Printf.sprintf "%2d" n) line
    in
    match cells with
    | [ a; b; c; d; e ] -> Printf.printf "%s %s %s %s %s\n" a b c d e
    | _ -> raise (Invalid_argument "boards must be 5 columns")
  in
  List.iter print_board_line board;
  print_char '\n'

let score board last_draw =
  let score_cell cell = Option.fold ~none:0 ~some:id cell in
  let score_row row =
    List.fold_left (fun acc cell -> acc + score_cell cell) 0 row
  in
  let cell_sum = List.fold_left (fun acc row -> acc + score_row row) 0 board in
  cell_sum * last_draw

let mark n board =
  let mark_cell cell =
    match cell with None -> None | Some c -> if c == n then None else Some c
  in

  let mark_row row = List.map mark_cell row in
  List.map mark_row board

let is_winner board =
  let is_winning_row = List.for_all Option.is_none in
  let winner =
    List.exists is_winning_row board
    || List.exists is_winning_row (transpose board)
  in
  winner

let part_1 () =
  let games = ref boards in
  let winner = ref None in
  List.iter
    (fun draw ->
      if Option.is_none !winner then (
        (* mark the boards *)
        games := List.map (mark draw) !games;

        (* check for winners *)
        match List.filter is_winner !games with
        | [] -> ()
        | board :: _ -> winner := Some (board, draw))
      else ())
    draws;
  match !winner with
  | Some (board, last_draw) -> score board last_draw
  | None -> raise (Failure "no winner found?")

let part_2 () =
  let games = ref boards in
  let winner = ref None in
  let draw_index = ref 0 in

  for i = 0 to List.length draws - 1 do
    let draw = List.nth draws i in

    if Option.is_none !winner then (
      draw_index := i;

      (* mark the boards *)
      games := List.map (mark draw) !games;

      (* check for only one non-winner *)
      match List.filter (not % is_winner) !games with
      | [ board ] -> winner := Some board
      | _ -> ())
  done;

  (* Okay so we know the winning board, but now we need to finish that game to
     get it's score. *)
  let board = ref (Option.get !winner) in

  for i = !draw_index to List.length draws - 1 do
    let draw = List.nth draws i in

    if not (is_winner !board) then (
      board := mark draw !board;
      draw_index := i)
  done;
  let last_draw = List.nth draws !draw_index in
  score !board last_draw

let print_answer () =
  Printf.printf "day 04 part 1: %d\n" (part_1 ());
  Printf.printf "day 04 part 2: %d\n" (part_2 ())

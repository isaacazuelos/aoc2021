open Advent_of_code.Util
open Advent_of_code.Io

let parse line =
  let to_2_tup list =
    match list with
    | [ left; right ] -> (left, right)
    | _ -> raise Parsing.Parse_error
  in

  let start, stop = to_2_tup (Str.split (Str.regexp " -> ") line) in
  let coord s =
    let x, y = to_2_tup (String.split_on_char ',' s) in
    (int_of_string x, int_of_string y)
  in
  (coord start, coord stop)

let print_coord (x, y) = Printf.printf "(%d, %d)" x y

let print_line ((x1, y1), (x2, y2)) =
  Printf.printf "%d,%d -> %d,%d\n" x1 y1 x2 y2

let input =
  let lines = read_lines "./inputs/day_05.txt" in
  List.map parse lines

let board_size =
  let fit (x1, y1) (x2, y2) = (max x1 x2, max y1 y2) in
  let x, y =
    List.fold_left
      (fun acc (start, stop) -> fit (fit acc start) stop)
      (0, 0) input
  in
  (* because of zero indexing *)
  (x + 1, y + 1)

let new_board () =
  let x, y = board_size in
  Array.make_matrix y x 0

let print_board board =
  let print_cell cell = Printf.printf "%d " cell in
  let print_row row =
    Array.iter print_cell row;
    print_newline ()
  in
  Array.iter print_row board

(* All the numbers from start to stop, _including_ stop. *)
let range start stop =
  let step = if start > stop then succ else pred in
  let rec go acc start stop =
    (* we count down so we don't have to reverse the list after *)
    if start = stop then stop :: acc else go (stop :: acc) start (step stop)
  in
  go [] start stop

type direction = Horizontal | Vertical | Diagonal

let direction_of ((x1, y1), (x2, y2)) =
  if x1 == x2 then Vertical else if y1 == y2 then Horizontal else Diagonal

let coords_in line =
  let h_coords ((x1, y), (x2, _)) = List.map (fun x -> (x, y)) (range x1 x2) in
  let v_coords ((x, y1), (_, y2)) = List.map (fun y -> (x, y)) (range y1 y2) in
  let d_coords ((x1, y1), (x2, y2)) = zip (range x1 x2) (range y1 y2) in

  let coords =
    match direction_of line with
    | Horizontal -> h_coords line
    | Vertical -> v_coords line
    | Diagonal -> d_coords line
  in

  coords

let darw_line board line =
  let draw_coord board (x, y) = board.(y).(x) <- succ board.(y).(x) in
  List.iter (draw_coord board) (coords_in line)

let overlaps board =
  let overlaps_row =
    Array.fold_left (fun acc cell -> if cell > 1 then succ acc else acc)
  in
  Array.fold_left overlaps_row 0 board

let part_1 () =
  let board = new_board () in
  let no_diagonals =
    List.filter (fun line -> Diagonal != direction_of line) input
  in
  List.iter (darw_line board) no_diagonals;
  overlaps board

let part_2 () =
  let board = new_board () in
  List.iter (darw_line board) input;
  overlaps board

let print_answer () =
  Printf.printf "day 04 part 1: %d\n" (part_1 ());
  Printf.printf "day 04 part 2: %d\n" (part_2 ())

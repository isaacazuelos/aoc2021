open Advent_of_code.Io
open Advent_of_code.Util

let input =
  let ascii_offset = int_of_char '0' in
  let lines = read_lines "./inputs/day_09.txt" in
  let parse_line line =
    Array.of_list
      (String.fold_right
         (fun c acc -> (int_of_char c - ascii_offset) :: acc)
         line [])
  in
  Array.of_list (List.map parse_line lines)

let max_y = Array.length input

let max_x = Array.length (Array.get input 0)

let height (x, y) = Array.get (Array.get input y) x

let is_higher_than p1 p2 = height p1 > height p2

let on_board (x, y) = x >= 0 && x < max_x && y >= 0 && y < max_y

let neighbours (x, y) =
  List.filter on_board [ (x, y + 1); (x - 1, y); (x + 1, y); (x, y - 1) ]

let risk point = height point + 1

let low_points =
  let points = ref [] in
  for y = 0 to max_y - 1 do
    for x = 0 to max_x - 1 do
      let cursor = (x, y) in
      let is_low =
        List.for_all (fun n -> is_higher_than n cursor) (neighbours cursor)
      in
      if is_low then points := (x, y) :: !points else ()
    done
  done;
  !points

let part_1 () = List.fold_left (fun acc point -> acc + risk point) 0 low_points

let map = Array.make_matrix max_y max_x None

let map_get (x, y) = Array.get (Array.get map y) x

let map_set n (x, y) =
  if height (x, y) < 9 && Option.is_none (map_get (x, y)) then
    map.(y).(x) <- Some n
  else ()

let map_print () =
  let print_cell cell =
    match cell with
    | None -> print_string "---- "
    | Some n -> Printf.printf "%4d " n
  in
  let print_row row =
    Array.iter print_cell row;
    print_newline ()
  in
  Array.iter print_row map

(* fill map *)
let iterate () =
  for y = 0 to max_y - 1 do
    for x = 0 to max_x - 1 do
      let cursor = (x, y) in
      match map_get cursor with
      | None -> ()
      | Some n -> List.iter (map_set n) (neighbours cursor)
    done
  done;
  ()

(* fill map *)
let () =
  List.iter (uncurry map_set) (enumerate low_points);
  (* iterate some fixed number of times to make sure the basins are full. Worse
     case can't be worse than this. *)
  for _i = 0 to max_x * max_y do
    iterate ()
    (* print_int i; *)
    (* map_print (); *)
    (* print_newline () *)
  done;
  ()

let matrix_count mat x =
  let sum_row row =
    Array.fold_left
      (fun acc cell -> if Option.equal Int.equal x cell then acc + 1 else acc)
      0 row
  in
  Array.fold_left (fun acc row -> acc + sum_row row) 0 mat

let part_2 () =
  let indexes = range 0 (List.length low_points) in
  let counts = List.map (fun i -> matrix_count map (Some i)) indexes in
  (* I'm slowly just making this Haskell... *)
  (prod % take 3 % List.sort (flip compare)) counts

let print_answer () =
  Printf.printf "day 09 part 1: %d\n" (part_1 ());
  Printf.printf "day 09 part 2: %d\n" (part_2 ())

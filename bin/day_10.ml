open Advent_of_code.Io
open Advent_of_code.Util

let input = read_lines "./inputs/day_10.txt"

let closing c =
  match c with
  | '(' -> ')'
  | '[' -> ']'
  | '{' -> '}'
  | '<' -> '>'
  | c ->
      raise @@ invalid_arg @@ Printf.sprintf "Invalid character in input %C" c

let score_line line =
  let next acc c =
    match acc with
    | Error e -> Error e
    | Ok stack -> (
        match c with
        | '(' | '[' | '{' | '<' -> Ok (closing c :: stack)
        | ')' | ']' | '}' | '>' -> (
            match stack with
            | [] -> Ok []
            | s :: ss -> if Char.equal c s then Ok ss else Error c)
        | e ->
            raise @@ invalid_arg
            @@ Printf.sprintf "Invalid character in input %C" e)
  in
  String.fold_left next (Ok []) line

let part_1 () =
  let score c =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ ->
        raise @@ invalid_arg @@ Printf.sprintf "Invalid character in input %C" c
  in

  List.fold_left
    (fun acc line ->
      acc + match score_line line with Ok _ -> 0 | Error n -> score n)
    0 input

let part_2 () =
  let score stack =
    let score acc c =
      (5 * acc)
      +
      match c with
      | ')' -> 1
      | ']' -> 2
      | '}' -> 3
      | '>' -> 4
      | _ ->
          raise @@ invalid_arg
          @@ Printf.sprintf "Invalid character in input %C" c
    in
    List.fold_left score 0 stack
  in

  let scores =
    (List.sort compare
    % List.filter (( != ) 0)
    % List.map (fun line ->
          match score_line line with
          | Ok remainder -> score remainder
          | Error _ -> 0))
      input
  in

  let n = List.length scores / 2 in

  List.nth scores n

let print_answer () =
  Printf.printf "day 10 part 1: %d\n" (part_1 ());
  Printf.printf "day 10 part 2: %d\n" (part_2 ())

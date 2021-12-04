(* Some utility funcitons I'll probaly want more than once. *)

let uncurry f (x, y) = f x y

(* Function composition *)
let ( % ) f g x = f (g x)

let id x = x

let rec window3 list =
  match list with
  | a :: b :: c :: xs -> (a, b, c) :: window3 (b :: c :: xs)
  | _ -> []

let rec zip xs ys =
  match (xs, ys) with
  | _, [] -> []
  | [], _ -> []
  | x :: xs, y :: ys -> (x, y) :: zip xs ys

let unzip pairs =
  List.fold_right (fun (x, y) (xs, ys) -> (x :: xs, y :: ys)) [] pairs

let count pred xs =
  let rec go acc xs =
    match xs with
    | [] -> acc
    | x :: xs -> if pred x then go (acc + 1) xs else go acc xs
  in
  go 0 xs

(* https://stackoverflow.com/questions/29607384/tranpose-a-list-list-in-ocaml *)
let rec transpose list =
  match list with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let int_of_bits bits =
  let go acc bit_is_set = (2 * acc) + if bit_is_set then 1 else 0 in
  List.fold_left go 0 bits
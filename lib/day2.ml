(* return a list of number pairs representing the beginning and ends of each ID range *)
let parse_ranges =
  let ranges =
    String.split_on_char ','
      (List.nth (In_channel.input_lines @@ open_in "resources/day2.txt") 0)
  in
  let rec parse_numbers build remaining =
    match remaining with
    | head :: tail ->
        let number_strings = String.split_on_char '-' head in
        parse_numbers
          (( List.nth number_strings 0 |> int_of_string,
             List.nth number_strings 1 |> int_of_string )
          :: build)
          tail
    | [] -> build
  in
  parse_numbers [] ranges

(* return a list of integers in the given range, inclusive. works in decreasing order as well *)
let range start last =
  let rec aux start last build =
    if last < start then build else aux start (last - 1) (last :: build)
  in
  if start < last then aux start last [] else List.rev (aux last start [])

let is_valid_id number part =
  let candidate = string_of_int number in

  let pattern =
    (* attempt to match two identical groups in the string *)
    if part = 1 then "^(.*)\\1$"
    (* attempt to match at least two identical groups in the string *)
      else "^(.*)\\1+$"
  in

  let expression = Pcre.regexp pattern in
  try
    ignore (Pcre.exec ~rex:expression candidate);
    false
  with Not_found -> true

(* find the sum of invalid IDs for the given list of IDs *)
let rec traverse_range sum list part =
  match list with
  | number :: tail ->
      if is_valid_id number part then traverse_range sum tail part
      else traverse_range (sum + number) tail part
  | [] -> sum

let rec traverse_list sum ranges part =
  match ranges with
  | (start, last) :: tail ->
      let range = range start last in
      traverse_list (sum + traverse_range 0 range part) tail part
  | [] -> sum

let run () =
  print_endline "DAY 2:";
  print_endline ("    Part 1: " ^ string_of_int (traverse_list 0 parse_ranges 1));
  print_endline ("    Part 2: " ^ string_of_int (traverse_list 0 parse_ranges 2))

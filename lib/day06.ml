(* return a sequence of either + or * operators *)
let parse_operator_line line =
  String.split_on_char ' ' line
  |> List.filter (fun item -> item <> "")
  |> List.map (function
    | "+" -> (( + ), 0)
    | "*" -> (( * ), 1)
    | _ -> failwith "Invalid operator!")
  |> List.to_seq

(* transform the rows of numbers into columns of numbers using transpose *)
let parse_number_lines_part1 lines =
  List.map
    (fun line ->
      String.split_on_char ' ' line
      |> List.filter (fun item -> item <> "")
      |> List.map (fun item -> int_of_string item)
      |> List.to_seq)
    lines
  |> List.to_seq |> Seq.transpose

(*
 * tbh i don't understand why this is needed but basically the column of 
 * numbers for each math problem ends up separated by a blank sequence,
 * so rebuild sane lists to do math with
 *)
let rec restructure build grid =
  match grid with
  | [] -> build
  | list :: tail ->
      (* we hit an empty list, this means we move onto the next column of numbers *)
      if List.is_empty list then restructure ([] :: build) tail
      else restructure ((List.hd list :: List.hd build) :: List.tl build) tail

let parse_number_lines_part2 lines =
  lines
  (* transform to (char seq seq) so it can be transposed *)
  |> List.map (fun line -> String.to_seq line)
  |> List.to_seq |> Seq.transpose
  |> Seq.map (fun line ->
      (* transform inner sequences back to lists, reverse them, and make strings *)
      line |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq
      (* split and get actual numbers *)
      |> String.split_on_char ' '
      |> List.filter (fun it -> it <> "")
      |> List.map (fun it -> int_of_string it))
  (* back to a list so we can restructure *)
  |> List.of_seq
  |> restructure [ [] ]
  (* finally back to sequence for doing the math *)
  |> List.map (fun row -> List.to_seq row)
  |> List.to_seq

let do_problem numbers operator start =
  Seq.fold_left (fun sum number -> operator sum number) start numbers

let do_problems number_lists operators =
  Seq.fold_left2
    (fun sum numbers (operator, start) ->
      sum + do_problem numbers operator start)
    0 number_lists operators

let run () =
  print_endline "DAY 6:";
  let lines =
    open_in "resources/day06.txt" |> In_channel.input_lines |> List.rev
  in

  let operators = List.hd lines |> parse_operator_line in
  let numbers = List.tl lines |> parse_number_lines_part1 in
  print_endline
    ("    Part 1: " ^ (do_problems numbers operators |> string_of_int));

  let numbers_part2 = List.tl lines |> parse_number_lines_part2 in
  let operators_part2 = operators |> List.of_seq |> List.rev |> List.to_seq in
  print_endline
    ("    Part 2: "
    ^ (do_problems numbers_part2 operators_part2 |> string_of_int))

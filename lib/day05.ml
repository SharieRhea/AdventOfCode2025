let parse_range line =
  let list = String.split_on_char '-' line in
  (List.nth list 0 |> int_of_string, List.nth list 1 |> int_of_string)

let combine_ranges ranges =
  let rec combine build = function
    | [] -> build
    | head :: tail -> (
        match build with
        | [] -> combine (head :: build) tail
        | curr_range :: good_ranges ->
            if fst head <= snd curr_range && fst head > snd curr_range then
              (* new range starts within the last working range and goes beyond it, combine the two ranges *)
              combine ((fst curr_range, snd head) :: good_ranges) tail
            else
              (* the new range comes after the last working range, append it *)
              combine (head :: build) tail)
  in
  combine [] (List.sort compare ranges)

let is_fresh id = List.exists (fun (first, last) -> id >= first && id <= last)

let count_available_fresh =
  List.fold_left (fun ids (first, last) -> ids + last - first + 1) 0

let run () =
  print_endline "DAY 5:";
  let ranges, ids =
    open_in "resources/day05.txt"
    |> In_channel.input_lines
    |> List.filter (fun line -> line <> "")
    |> List.partition_map (fun line ->
        if String.contains line '-' then Left (parse_range line)
        else Right (int_of_string line))
  in
  let ranges = combine_ranges ranges in
  let fresh_ids =
    List.fold_left
      (fun sum id -> if is_fresh id ranges then sum + 1 else sum)
      0 ids
  in
  print_endline ("    Part 1: " ^ string_of_int fresh_ids);
  print_endline ("    Part 2: " ^ string_of_int @@ count_available_fresh ranges)

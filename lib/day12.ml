type region = { dimensions : int * int; quantities : int list }

let parse_region line =
  let split_once sep s =
    match String.split_on_char sep s with
    | [ a; b ] -> (a, b)
    | _ -> failwith "Too many elements!"
  in
  let dims, qtys = split_once ':' line in
  let dimensions =
    match split_once 'x' dims with
    | width, height -> (int_of_string width, int_of_string height)
  in
  let quantities =
    qtys |> String.split_on_char ' '
    |> List.filter_map (fun s ->
        if s = "" then None else Some (int_of_string s))
  in
  { dimensions; quantities }

(* assume that each shape is a full 3x3, will they fit just placing side-by-side? *)
let check_lazy_pack region =
  let present_count = region.quantities |> List.fold_left ( + ) 0 in
  let columns = fst region.dimensions / 3 in
  let rows = snd region.dimensions / 3 in
  present_count <= columns * rows

let run () =
  print_endline "DAY 12:";
  (* turns out that you don't need to use the shapes at all...
   don't bother parsing them i guess *)
  let valid_regions =
    open_in "resources/day12.txt"
    |> In_channel.input_lines
    |> List.filter (fun line -> String.contains line 'x')
    |> List.map parse_region
    |> List.fold_left
         (fun sum region -> if check_lazy_pack region then sum + 1 else sum)
         0
  in
  print_endline ("    Part 1: " ^ (valid_regions |> string_of_int))

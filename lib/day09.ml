let parse_point line =
  let numbers =
    line |> String.split_on_char ','
    |> List.map (fun number -> int_of_string number)
  in
  (List.hd numbers, List.nth numbers 1)

let calculate_areas points =
  List.mapi
    (fun index (x1, y1) ->
      List.map
        (fun (x2, y2) -> (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1))
        (* make sure not to double count pairs *)
        (List.drop (index + 1) points))
    points
  |> List.flatten
  |> List.sort (Fun.flip compare)

let run () =
  print_endline "DAY 9:";
  let red_tiles =
    open_in "resources/day09.txt"
    |> In_channel.input_lines |> List.map parse_point
  in
  let largest_area = red_tiles |> calculate_areas |> List.hd in
  print_endline ("    Part 1: " ^ string_of_int largest_area);
  print_endline ("    Part 2: " ^ string_of_int 0)

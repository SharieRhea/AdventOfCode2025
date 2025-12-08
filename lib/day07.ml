module IntPair = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match compare x0 x1 with 0 -> compare y0 y1 | c -> c
end

module IntPairSet = Set.Make (IntPair)

(* return a set of (x, y) coordinates for '^' tiles and the starting tile coordinates *)
let parse_splitters lines =
  (* generate a single list of ((x, y), char) to represent each tile *)
  let tiles =
    List.mapi
      (fun y line ->
        List.mapi
          (fun x character -> ((x, y), character))
          (line |> String.to_seq |> List.of_seq))
      lines
    |> List.concat
  in
  (* filter for only '^' tiles and convert to a set *)
  let splitters =
    List.map
      (fun tile -> fst tile)
      (List.filter (fun tile -> snd tile = '^') tiles)
    |> IntPairSet.of_list
  in
  (* find the tile where the beam originates *)
  let start = List.find (fun (_, character) -> character = 'S') tiles in
  (splitters, fst start)

(* add beams to the table, accounting for beams already on that tile *)
let add_beams table key beams =
  match Hashtbl.find_opt table key with
  | None -> Hashtbl.add table key beams
  | Some existing -> Hashtbl.replace table key (existing + beams)

let trace_beam splitters start max_x max_y : int * int =
  let table = Hashtbl.create 32 in
  let activated = ref IntPairSet.empty in
  Hashtbl.add table start 1;

  (* imperative, i'm sorry :( *)
  for y = 0 to max_y - 1 do
    for x = 0 to max_x - 1 do
      match Hashtbl.find_opt table (x, y - 1) with
      | None -> () (* there were no beams above, do nothing *)
      | Some incoming ->
          if IntPairSet.mem (x, y) splitters then (
            (* encountered splitter, add to set and move beams to either side *)
            activated := IntPairSet.add (x, y) !activated;
            let left, right = ((x - 1, y), (x + 1, y)) in
            add_beams table left incoming;
            add_beams table right incoming)
          else
            (* no splitter, just move beams down to this tile *)
            add_beams table (x, y) incoming
    done
  done;

  (* sum all the beams on the last row to find the number of timelines *)
  let timelines =
    Hashtbl.fold
      (fun (_, y) beams sum -> if y = max_y - 1 then sum + beams else sum)
      table 0
  in
  (!activated |> IntPairSet.cardinal, timelines)

let run () =
  print_endline "DAY 7:";
  let lines = In_channel.input_lines (open_in "resources/day07.txt") in
  let max_x, max_y =
    (lines |> List.hd |> String.length, lines |> List.length)
  in

  let splitters, start = parse_splitters lines in
  let activated, timelines = trace_beam splitters start max_x max_y in
  print_endline ("    Part 1: " ^ string_of_int activated);
  print_endline ("    Part 2: " ^ string_of_int timelines)

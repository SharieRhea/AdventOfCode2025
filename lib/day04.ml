module IntPairs = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match compare x0 x1 with 0 -> compare y0 y1 | c -> c
end

module PairsSet = Set.Make (IntPairs)

(* directional vectors used for finding adjacent tiles *)
let vectors =
  [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]

let generate_adjacent coord =
  List.map
    (fun vector -> (fst coord + fst vector, snd coord + snd vector))
    vectors

(* return a set of (x, y) coordinates for '@' tiles *)
let parse_rolls_set =
  let lines = In_channel.input_lines (open_in "resources/day04.txt") in
  let tiles =
    (* generate a single list of ((x, y), char) to represent each tile *)
    List.concat
    @@ List.mapi
         (fun y line ->
           List.mapi
             (fun x character -> ((x, y), character))
             (line |> String.to_seq |> List.of_seq))
         lines
  in
  (* filter for only '@' tiles and convert to a set *)
  PairsSet.of_list
  @@ List.map
       (fun tile -> fst tile)
       (List.filter (fun tile -> snd tile = '@') tiles)

(* a roll of paper is accessible if less than 4 adjacent tiles are occupied *)
let is_accessible coordinate set =
  let accumulate sum tile = if PairsSet.mem tile set then sum + 1 else sum in
  List.fold_left accumulate 0 (generate_adjacent coordinate) < 4

let find_accessible set =
  (* create a set of tiles that have a roll of paper *)
  let rec traverse build = function
    | head :: tail ->
        traverse (if is_accessible head set then head :: build else build) tail
    | [] -> build
  in
  traverse [] (PairsSet.to_list set)

(* continue to remove iterations of accessible rolls until no more can be removed *)
let rec remove_rolls occupied_set sum =
  let accessible = find_accessible occupied_set |> PairsSet.of_list in
  if PairsSet.cardinal accessible = 0 then sum
  else
    remove_rolls
      (PairsSet.diff occupied_set accessible)
      (sum + PairsSet.cardinal accessible)

let run () =
  print_endline "DAY 4:";
  let part_1 = find_accessible parse_rolls_set |> List.length in
  print_endline ("    Part 1: " ^ string_of_int part_1);
  let part_2 = remove_rolls parse_rolls_set 0 in
  print_endline ("    Part 2: " ^ string_of_int part_2)

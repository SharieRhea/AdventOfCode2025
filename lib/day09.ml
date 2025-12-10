module IntPair = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match compare x0 x1 with 0 -> compare y0 y1 | c -> c
end

module IntPairSet = Set.Make (IntPair)
module IntSet = Set.Make (Int)

let parse_point line =
  match String.split_on_char ',' line |> List.map int_of_string with
  | [ x; y ] -> (x, y)
  | _ -> failwith ("Invalid point: " ^ line)

let connect_points (x1, y1) (x2, y2) =
  if x1 = x2 then List.init (abs (y2 - y1) + 1) (fun i -> (x1, min y1 y2 + i))
  else List.init (abs (x2 - x1) + 1) (fun i -> (min x1 x2 + i, y1))

let build_perimeter points =
  let rec loop build = function
    | a :: (b :: _ as tail) -> loop (connect_points a b @ build) tail
    | [ last ] ->
        (* connect last back to first *)
        connect_points last (List.hd points) @ build
    | [] -> build
  in
  IntPairSet.of_list (loop [] points)

let check_bounds (x1, y1) (x2, y2) filled =
  (* create the four corners and connect them *)
  let corners = [ (x1, y1); (x1, y2); (x2, y2); (x2, y1) ] in
  let perimeter = build_perimeter corners in
  let rec check = function
    | [] -> true
    | head :: tail ->
        if not (IntPairSet.mem head filled) then false else check tail
  in
  check (IntPairSet.to_list perimeter)

(* i know i could use a map for this... i'm tired and it works *)
let get_compressed_key points =
  (* split xs and ys into two sets of unique values *)
  let xs, ys = points |> List.split in
  let xs = xs |> IntSet.of_list |> IntSet.to_list |> List.sort compare in
  let ys = ys |> IntSet.of_list |> IntSet.to_list |> List.sort compare in
  (xs, ys)

let compress_space (xs, ys) points =
  (* use the index of a value as the new coordinate *)
  points
  |> List.map (fun (x, y) ->
      ( Option.get (List.find_index (fun i -> i = x) xs),
        Option.get (List.find_index (fun i -> i = y) ys) ))

let uncompress_area (x1, y1) (x2, y2) (x_key, y_key) =
  (* first convert to original space coordinates *)
  let x1 = List.nth x_key x1 in
  let x2 = List.nth x_key x2 in
  let y1 = List.nth y_key y1 in
  let y2 = List.nth y_key y2 in
  (* the area for the box defined by this diagonal *)
  (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

let find_interior_point perimeter =
  let rec move (x, y) crossed =
    (* continuously move right and down until we've crossed the perimeter and one further *)
    let on_perimeter = IntPairSet.mem (x + 1, y + 1) perimeter in
    if not crossed then
      if on_perimeter then move (x + 1, y + 1) true
      else move (x + 1, y + 1) false
    else if on_perimeter then move (x + 1, y + 1) true
    else (x + 1, y + 1)
  in
  move (0, 0) false

let flood_fill perimeter (x_start, y_start) =
  let queue = Queue.create () in
  Queue.add (x_start, y_start) queue;

  let rec bfs filled =
    match Queue.take_opt queue with
    | None -> filled
    | Some (x, y) ->
        (* explore the 4 neighbors *)
        let filled =
          List.fold_left
            (fun filled (dx, dy) ->
              let nx, ny = (x + dx, y + dy) in
              if not (IntPairSet.mem (nx, ny) filled) then (
                Queue.add (nx, ny) queue;
                IntPairSet.add (nx, ny) filled)
              else filled)
            filled
            [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
        in
        bfs filled
  in

  let filled = IntPairSet.add (x_start, y_start) perimeter in
  bfs filled

let part1 points =
  List.mapi
    (fun index (x1, y1) ->
      List.map
        (fun (x2, y2) -> (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1))
        (* make sure not to double count pairs *)
        (List.drop (index + 1) points))
    points
  |> List.flatten
  |> List.sort (Fun.flip compare)
  |> List.hd

let part2 points =
  (* build a smaller compressed space to check bounds with *)
  let key = get_compressed_key points in
  let points = compress_space key points in
  let perimeter = build_perimeter points in
  let start = find_interior_point perimeter in
  let filled = flood_fill perimeter start in

  (* find all corner pairs that are valid using the compressed spaced *)
  List.mapi
    (fun index p1 ->
      List.map
        (fun p2 -> if check_bounds p1 p2 filled then (p1, p2) else (p1, p1))
        (List.drop (index + 1) points))
    points
  |> List.flatten
  |> List.filter (fun (p1, p2) -> p1 <> p2)
    (* find area of each valid box and get largest *)
  |> List.map (fun (p1, p2) -> uncompress_area p1 p2 key)
  |> List.sort (Fun.flip compare)
  |> List.hd

let run () =
  print_endline "DAY 9:";
  let points =
    open_in "resources/day09.txt"
    |> In_channel.input_lines |> List.map parse_point
  in
  print_endline ("    Part 1: " ^ (points |> part1 |> string_of_int));
  print_endline ("    Part 2: " ^ (points |> part2 |> string_of_int))

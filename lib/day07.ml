type node = { position : int * int; id : int }

module IntPair = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match compare x0 x1 with 0 -> compare y0 y1 | c -> c
end

module IntPairSet = Set.Make (IntPair)

module Node = struct
  type t = node

  let compare node1 node2 =
    match compare node1.position node2.position with
    | 0 -> compare node1.id node2.id
    | c -> c
end

module NodeSet = Set.Make (Node)

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

let trace_beam splitters start max_height =
  let queue = Queue.create () in
  Queue.push start queue;

  (* imperative queue :( *)
  let rec trace activated_splitters visited : int =
    if Queue.is_empty queue then
      (IntPairSet.cardinal activated_splitters)
    else
      let x, y = Queue.pop queue in
      (* if we've passed all possible splitters, this beam no longer matters *)
      if y > max_height then trace activated_splitters visited
        (* encountering a splitter, create two new positions to follow *)
      else if IntPairSet.mem (x, y + 1) splitters then
        let new_splitters = IntPairSet.add (x, y + 1) activated_splitters in
        match ((x - 1, y + 1), (x + 1, y + 1)) with
        | left, right
          when (not (IntPairSet.mem left visited))
               && not (IntPairSet.mem right visited) ->
            Queue.push left queue;
            Queue.push right queue;
            trace new_splitters
              (visited |> IntPairSet.add left |> IntPairSet.add right)
        | left, _ when not @@ IntPairSet.mem left visited ->
            Queue.push left queue;
            trace new_splitters (visited |> IntPairSet.add left)
        | _, right when not @@ IntPairSet.mem right visited ->
            Queue.push right queue;
            trace new_splitters (visited |> IntPairSet.add right)
        | _ -> trace new_splitters visited
      (* just empty space, continue moving *)
        else (
        Queue.push (x, y + 1) queue;
        let visited = IntPairSet.add (x, y + 1) visited in
        trace activated_splitters visited)
  in
  let empty_visited = IntPairSet.of_list [ start ] in
  trace IntPairSet.empty empty_visited

let run () =
  print_endline "DAY 5:";
  let lines = In_channel.input_lines (open_in "resources/test.txt") in
  let splitters, start = parse_splitters lines in
  let max_height = List.length lines in

  let activated_splitters = trace_beam splitters start max_height in
  print_endline ("    Part 1: " ^ string_of_int activated_splitters);
  print_endline ("    Part 2: " ^ string_of_int 0)

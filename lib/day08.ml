type point = { x : int; y : int; z : int }
type box_pair = { distance : int; point1 : point; point2 : point }

let distance point1 point2 =
  let square x = x * x in
  square (point2.x - point1.x)
  + square (point2.y - point1.y)
  + square (point2.z - point1.z)

let parse_point line =
  let numbers =
    line |> String.split_on_char ','
    |> List.map (fun number -> int_of_string number)
  in
  { x = List.nth numbers 0; y = List.nth numbers 1; z = List.nth numbers 2 }

let calculate_distances points =
  List.mapi
    (fun index point1 ->
      List.map
        (fun point2 -> { distance = distance point1 point2; point1; point2 })
        (* make sure not to double count pairs *)
        (List.drop (index + 1) points))
    points
  (* sort for shortest distance first *)
  |> List.flatten
  |> List.sort compare

let encode list =
  let rec aux count build remaining =
    match remaining with
    | [] -> []
    | [ x ] -> (count + 1, x) :: build
    | a :: (b :: _ as tail) ->
        if a = b then aux (count + 1) build tail
        else aux 0 ((count + 1, a) :: build) tail
  in
  List.rev (aux 0 [] list)

(* update the ids of an entire circuit *)
let update_circuit table old_id new_id =
  table |> Hashtbl.to_seq
  |> Seq.filter_map (fun (key, value) ->
      if value = old_id then Some key else None)
  |> Seq.iter (fun binding -> Hashtbl.replace table binding new_id)

let join_pair table box_pair =
  let id1 = Hashtbl.find table box_pair.point1 in
  let id2 = Hashtbl.find table box_pair.point2 in
  if id1 < id2 then update_circuit table id2 id1
  else update_circuit table id1 id2

let build_circuits points box_pairs number_of_pairs =
  let table = Hashtbl.create (List.length points) in
  (* add each box to the table as its own circuit with a unique id *)
  points |> List.iteri (fun id point -> Hashtbl.add table point id);

  box_pairs |> List.take number_of_pairs
  |> List.iter (fun box_pair -> join_pair table box_pair);

  (* take all the table values (circuit ids) and figure out their lengths *)
  let circuit_lengths, _ =
    table |> Hashtbl.to_seq_values |> List.of_seq |> List.sort compare |> encode
    |> List.split
  in
  (* multiple 3 largest circuits *)
  circuit_lengths
  |> List.sort (Fun.flip compare)
  |> List.take 3 |> List.fold_left ( * ) 1

let build_master_circuit points box_pairs =
  let table = Hashtbl.create (List.length points) in
  points |> List.iteri (fun id point -> Hashtbl.add table point id);
  let index = ref 0 in

  (* continue connecting pairs until there is only 1 circuit id left *)
  while
    table |> Hashtbl.to_seq_values |> List.of_seq |> encode |> List.length > 1
  do
    join_pair table (List.nth box_pairs !index);
    index := !index + 1
  done;

  (* get the last pair of boxes that were connected and their x values *)
  let pair = List.nth box_pairs (!index - 1) in
  pair.point1.x * pair.point2.x

let run () =
  print_endline "DAY 8:";
  let points =
    In_channel.input_lines (open_in "resources/day08.txt")
    |> List.map parse_point
  in
  let box_pairs = calculate_distances points in
  let top_three = build_circuits points box_pairs 1000 in
  print_endline ("    Part 1: " ^ string_of_int top_three);
  let last_junction = build_master_circuit points box_pairs in
  print_endline ("    Part 2: " ^ string_of_int last_junction)

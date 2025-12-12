module StringMap = Map.Make (String)

let parse_device line =
  match line |> String.split_on_char ':' with
  | [ key; outputs ] -> (key, outputs |> String.split_on_char ' ' |> List.tl)
  | _ -> failwith "Invalid format!"

let part1 map =
  let cache = Hashtbl.create 10 in

  let rec dfs key =
    match key with
    | "out" -> 1
    | _ -> (
        match Hashtbl.find_opt cache key with
        | Some paths -> paths
        | None ->
            (* recursively walk all branching paths *)
            let paths =
              StringMap.find key map
              |> List.fold_left (fun sum output -> sum + dfs output) 0
            in
            (* cache this node's paths to the output device so we never re-walk it *)
            Hashtbl.add cache key paths;
            paths)
  in
  dfs "you"

let part2 map =
  let cache = Hashtbl.create 10 in

  let rec dfs key fft dac =
    match key with
    (* a path is only valid if it visits BOTH fft and dac *)
    | "out" -> if fft && dac then 1 else 0
    | _ -> (
        (* add fft and dac status to the key because there are many paths to a given node,
           some may have passed fft or dac and some may not *)
        let cache_key = key ^ string_of_bool fft ^ string_of_bool dac in
        match Hashtbl.find_opt cache cache_key with
        | Some paths -> paths
        | None ->
            let fft = fft || key = "fft" in
            let dac = dac || key = "dac" in
            (* recursively walk all branching paths *)
            let paths =
              StringMap.find key map
              |> List.fold_left (fun sum output -> sum + dfs output fft dac) 0
            in
            (* cache this node's paths to the output device so we never re-walk it *)
            Hashtbl.add cache cache_key paths;
            paths)
  in
  dfs "svr" false false

let run () =
  Printexc.record_backtrace true;
  print_endline "DAY 11:";
  let map =
    open_in "resources/day11.txt"
    |> In_channel.input_lines |> List.map parse_device |> StringMap.of_list
  in
  let paths = part1 map in
  print_endline ("    Part 1: " ^ (paths |> string_of_int));
  let fft_dac_paths = part2 map in
  print_endline ("    Part 2: " ^ (fft_dac_paths |> string_of_int))

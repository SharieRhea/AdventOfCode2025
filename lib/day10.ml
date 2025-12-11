type machine = { target : BatBitSet.t; buttons : BatBitSet.t list }
type state = { presses : int; lights : BatBitSet.t }

module State = struct
  type t = state

  let compare state1 state2 = compare state1 state2
end

module StatePqueue = Pqueue.MakeMin (State)

let parse_lights config =
  let set = BatBitSet.empty () in
  config
  |> String.iteri (fun index character ->
      (* convert to bitset based on the lights that are turned on *)
      match character with '#' -> BatBitSet.set set (index - 1) | _ -> ());
  set

let parse_buttons config =
  let set = BatBitSet.empty () in
  config |> String.to_seq
  (* remove the opening and closing parentheses *)
  |> Seq.filter (fun it -> it <> '(' && it <> ')')
  |> String.of_seq |> String.split_on_char ','
  (* convert to bitset based on the buttons pressed *)
  |> List.iter (fun index -> BatBitSet.set set (int_of_string index));
  set

let build_machine line =
  let items = line |> String.split_on_char ' ' in
  let target = parse_lights (List.hd items) in
  let buttons =
    (* ignore the first item (lights config) and last item (joltage config) *)
    items |> List.tl
    |> List.take (List.length items - 2)
    |> List.map parse_buttons
  in
  { target; buttons }

let configure machine =
  let queue = StatePqueue.create () in
  let rec bfs = function
    | Some node ->
        (* apply every button to the current state and add to queue *)
        let rec apply_button = function
          | button :: tail ->
              let new_state = BatBitSet.sym_diff node.lights button in
              if BatBitSet.count new_state = 0 then node.presses + 1
              else (
                StatePqueue.add queue
                  { lights = new_state; presses = node.presses + 1 };
                apply_button tail)
          | [] -> bfs (StatePqueue.pop_min queue)
        in
        apply_button machine.buttons
    | None -> failwith "Did not reach target state!"
  in
  StatePqueue.add queue { lights = machine.target; presses = 0 };
  bfs (StatePqueue.pop_min queue)

let run () =
  print_endline "DAY 10:";
  let machines =
    open_in "resources/day10.txt"
    |> In_channel.input_lines |> List.map build_machine
  in

  let presses =
    machines
    |> List.fold_left (fun sum machine -> sum + (machine |> configure)) 0
  in
  print_endline ("    Part 1: " ^ (presses |> string_of_int));
  print_endline ("    Part 2: " ^ (0 |> string_of_int))

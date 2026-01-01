type machine = {
  diagram : BatBitSet.t;
  buttons : BatBitSet.t list;
  joltages : int list;
}

module StringTable = Hashtbl.Make (String)

(* PARSING *)
let parse_diagram config =
  let set = BatBitSet.empty () in
  config
  |> String.iteri (fun index character ->
      (* convert to bitset based on the lights that are turned on *)
      match character with
      | '#' -> BatBitSet.set set (index - 1)
      | _ -> ());
  set

let parse_buttons config =
  let set = BatBitSet.empty () in
  config |> String.to_seq
  (* remove the opening and closing parentheses *)
  |> Stdlib.Seq.filter (fun it -> it <> '(' && it <> ')')
  |> String.of_seq |> String.split_on_char ','
  (* convert to bitset based on the buttons pressed *)
  |> List.iter (fun index -> BatBitSet.set set (int_of_string index));
  set

let parse_joltages config =
  config |> String.to_seq
  (* remove the opening and closing braces *)
  |> Stdlib.Seq.filter (fun it -> it <> '{' && it <> '}')
  |> String.of_seq |> String.split_on_char ','
  (* convert to integers *)
  |> List.map int_of_string

let build_machine line =
  let items = line |> String.split_on_char ' ' in
  let diagram = parse_diagram (List.hd items) in
  let buttons =
    (* ignore the first item (lights config) and last item (joltage config) *)
    items |> List.tl
    |> List.take (List.length items - 2)
    |> List.map parse_buttons
  in
  let joltages = items |> List.rev |> List.hd |> parse_joltages in
  { diagram; buttons; joltages }

(* UTILITIES *)
let apply_button start button = BatBitSet.sym_diff start button
let apply_buttons start buttons = buttons |> List.fold_left apply_button start

let get_parity joltages =
  let set = BatBitSet.empty () in
  joltages
  |> List.iteri (fun index joltage ->
      BatBitSet.put set (joltage mod 2 = 1) index);
  set

let rec combinations = function
  | [] -> [ [] ]
  | head :: tail ->
      let with_head = List.map (fun l -> head :: l) (combinations tail) in
      let without_head = combinations tail in
      with_head @ without_head

let get_patterns buttons =
  let combos = combinations buttons in
  combos
  |> List.map (fun combo -> (apply_buttons (BatBitSet.empty ()) combo, combo))

let apply_buttons buttons joltages =
  joltages
  |> List.mapi (fun index joltage ->
      joltage
      - (buttons
        |> List.fold_left
             (fun sum button ->
               if BatBitSet.mem button index then sum + 1 else sum)
             0))

let min_opt a b =
  match (a, b) with None, x | x, None -> x | Some x, Some y -> Some (min x y)

let min_list = function [] -> None | x :: xs -> Some (List.fold_left min x xs)

let to_string_joltages joltages =
  let str =
    List.fold_left (fun acc it -> acc ^ string_of_int it ^ ",") "" joltages
  in
  "{" ^ str ^ "}"

(* PART 1 *)
let configure_machine machine =
  get_patterns machine.buttons
  |> List.filter_map (fun (pattern, buttons) ->
      if BatBitSet.equal machine.diagram pattern then Some (List.length buttons)
      else None)
  |> List.sort compare |> List.hd

(*
 * PART 2 
 * this sh!t is not fast but hey it's cooler than using z3   
 *)
let rec configure_joltage patterns memo joltages =
  let key = to_string_joltages joltages in
  match StringTable.find_opt memo key with
  | Some result -> result
  | None ->
      StringTable.add memo key None;

      (* pessimistic insert *)
      let result =
        if List.exists (fun j -> j < 0) joltages then None
        else if List.for_all (( = ) 0) joltages then Some 0
        else if List.exists (fun j -> j mod 2 = 1) joltages then
          (* odd parity *)
          let parity = get_parity joltages in
          patterns
          |> List.filter_map (fun (pattern, buttons) ->
              if BatBitSet.equal parity pattern then
                let next_joltages = apply_buttons buttons joltages in
                match configure_joltage patterns memo next_joltages with
                | None -> None
                | Some cost -> Some (cost + List.length buttons)
              else None)
          |> min_list
        else
          (* all even, allow at most 1 attempt at keeping the same parity but
             pressing buttons to prevent unsolvable machines *)
          let align_then_divide =
            patterns
            |> List.filter_map (fun (pattern, buttons) ->
                if BatBitSet.equal (BatBitSet.empty ()) pattern then
                  let after_press = apply_buttons buttons joltages in
                  let half = List.map (fun j -> j / 2) after_press in
                  match configure_joltage patterns memo half with
                  | None -> None
                  | Some cost -> Some ((cost * 2) + List.length buttons)
                else None)
            |> min_list
          in

          let divide_now =
            let half = List.map (fun j -> j / 2) joltages in
            match configure_joltage patterns memo half with
            | None -> None
            | Some cost -> Some (cost * 2)
          in

          min_opt align_then_divide divide_now
      in

      StringTable.replace memo key result;
      result

(* RUN *)
let part1 machines =
  List.fold_left (fun sum machine -> sum + configure_machine machine) 0 machines

let part2 machines =
  List.fold_left
    (fun sum machine ->
      let memo = StringTable.create 4096 in
      match
        configure_joltage (get_patterns machine.buttons) memo machine.joltages
      with
      | None -> failwith "No solution!"
      | Some minimum -> sum + minimum)
    0 machines

let run () =
  print_endline "DAY 10:";

  let machines =
    open_in "resources/day10.txt"
    |> In_channel.input_lines |> List.map build_machine
  in

  print_endline ("    Part 1: " ^ (machines |> part1 |> string_of_int));
  print_endline ("    Part 2: " ^ (machines |> part2 |> string_of_int))

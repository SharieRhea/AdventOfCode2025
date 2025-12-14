open Z3

type machine = {
  lights : BatBitSet.t;
  buttons : BatBitSet.t list;
  joltages : int list;
}

type state = { presses : int; lights : BatBitSet.t }
type system = { coefficients : int array array; augment : int array }

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
  let lights = parse_lights (List.hd items) in
  let buttons =
    (* ignore the first item (lights config) and last item (joltage config) *)
    items |> List.tl
    |> List.take (List.length items - 2)
    |> List.map parse_buttons
  in
  let joltages = items |> List.rev |> List.hd |> parse_joltages in
  { lights; buttons; joltages }

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
  StatePqueue.add queue { lights = machine.lights; presses = 0 };
  bfs (StatePqueue.pop_min queue)

(* begin part 2 hell *)

let transpose matrix =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  Array.init cols (fun j -> Array.init rows (fun i -> matrix.(i).(j)))

let convert_to_system machine =
  (* build a matrix where each row is a joltage value equation and each column represents a
     potential button to press *)
  let coefficients =
    Array.of_list
      (List.map
         (fun bitset ->
           Array.init (List.length machine.joltages) (fun i ->
               if BatBitSet.mem bitset i then 1 else 0))
         machine.buttons)
    |> transpose
  in
  (* augmented column of the matrix *)
  let augment = Array.of_list machine.joltages in
  { coefficients; augment }

(* use Z3 to solve a system of linear equations with more unknowns than
 * equations, while keeping all solutions integers values and mimimizing
 * their combined sum

 * i'm not even gonna pretend that i wrote this, although i do think i
 * understand the underlying math a little
 *)
let solve_min_sum (sys : system) =
  (* create the context for z3 to operate within *)
  let context = mk_context [] in
  let opt = Optimize.mk_opt context in

  (* dimensions of the A matrix in Ax = b *)
  let rows = Array.length sys.coefficients in
  let columns = Array.length sys.coefficients.(0) in

  (* make sure number of rows equals number of joltage values *)
  if rows <> Array.length sys.augment then failwith "Dimension mismatch!";

  (* create integer variables x0, x1, ... to represent number of presses
     for each button *)
  let vars =
    Array.init columns (fun i ->
        Arithmetic.Integer.mk_const_s context ("x" ^ string_of_int i))
  in
  (* constrain to >= 0 since you can't press a button negative times *)
  Array.iter
    (fun v ->
      let zero = Arithmetic.Integer.mk_numeral_i context 0 in
      Optimize.add opt [ Arithmetic.mk_ge context v zero ] |> ignore)
    vars;
  (* constrain Ax = b, basically attach the augment column *)
  for i = 0 to rows - 1 do
    let lhs =
      Array.fold_left
        (fun acc j ->
          let coeff = sys.coefficients.(i).(j) in
          if coeff = 0 then acc
          else
            let c = Arithmetic.Integer.mk_numeral_i context coeff in
            let term = Arithmetic.mk_mul context [ c; vars.(j) ] in
            match acc with
            | None -> Some term
            | Some e -> Some (Arithmetic.mk_add context [ e; term ]))
        None
        (Array.init columns Fun.id)
    in
    let lhs = Option.get lhs in
    let rhs = Arithmetic.Integer.mk_numeral_i context sys.augment.(i) in
    let eq = Boolean.mk_eq context lhs rhs in
    Optimize.add opt [ eq ] |> ignore
  done;

  (* minimize the sum of all xi so we have the least button presses possible *)
  let sum = Arithmetic.mk_add context (Array.to_list vars) in
  Optimize.minimize opt sum |> ignore;

  (* see if there is a solution *)
  match Optimize.check opt with
  | Solver.SATISFIABLE ->
      let model = Option.get (Optimize.get_model opt) in
      let solution =
        Array.map
          (fun v ->
            match Model.eval model v true with
            | Some n -> Arithmetic.Integer.get_big_int n
            | None -> failwith "Model error!")
          vars
      in
      (* return the sum of all button presses *)
      Array.fold_left (fun sum num -> sum + Z.to_int num) 0 solution
  | _ -> failwith "No solution!"

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

  let systems = List.map convert_to_system machines in
  let joltage_presses =
    List.fold_left (fun sum system -> sum + solve_min_sum system) 0 systems
  in
  print_endline ("    Part 2: " ^ (joltage_presses |> string_of_int))

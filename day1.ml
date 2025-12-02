(* the dial will never point to a negative number, wrap around *)
let positive_mod dividend divisor =
  let result = dividend mod divisor in
  if result < 0 then result + divisor else result

(* return plus or minus for moving right or left around dial *)
let get_operator character =
  match character with
  | 'R' -> ( + )
  | 'L' -> ( - )
  | _ -> failwith "Invalid rotation direction!"

(* count the number of times we land on 0 *)
let rec part_1 lines dial num_zeroes =
  match lines with
  | head :: tail ->
      let rotation = head.[0] in
      let distance =
        int_of_string (String.sub head 1 (String.length head - 1))
      in
      let new_dial = positive_mod ((get_operator rotation) dial distance) 100 in
      if new_dial = 0 then part_1 tail new_dial (num_zeroes + 1)
      else part_1 tail new_dial num_zeroes
  | [] -> num_zeroes

(* count the number of times we pass OR land on 0 *)
let rec part_2 lines dial num_zeroes =
  match lines with
  | head :: tail ->
      let rotation = head.[0] in
      let distance =
        int_of_string (String.sub head 1 (String.length head - 1))
      in

      (* figure out full rotations and then anything left over *)
      let passes = distance / 100 in
      let remainder = distance - (passes * 100) in

      let new_dial = positive_mod ((get_operator rotation) dial distance) 100 in

      (* handle weird edge cases regarding directions and landing on 0 *)
      let additional =
        if dial = 0 && rotation = 'L' then 0
        else if new_dial = 0 && rotation = 'L' then 1
        else if dial = 99 && rotation = 'R' then 1
        else if rotation = 'R' && dial + remainder > 99 then 1
        else if rotation = 'L' && dial - remainder < 0 then 1
        else 0
      in

      let new_num_zeroes = num_zeroes + passes + additional in
      part_2 tail new_dial new_num_zeroes
  | [] -> num_zeroes

let () =
  (* the dial starts pointing at 50 *)
  print_endline
    ("Part 1: "
    ^ string_of_int
        (part_1 (In_channel.input_lines (open_in "resources/day1.txt")) 50 0));
  print_endline
    ("Part 2: "
    ^ string_of_int
        (part_2 (In_channel.input_lines (open_in "resources/day1.txt")) 50 0))

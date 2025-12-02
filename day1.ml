let positive_mod dividend divisor =
  let result = dividend mod divisor in
  if result < 0 then (result + divisor, true) else (result, false)

let get_operator character =
  match character with
  | 'R' -> ( + )
  | 'L' -> ( - )
  | _ -> failwith "Invalid rotation direction!"

let rec part_1 lines dial num_zeroes =
  match lines with
  | head :: tail ->
      let rotation = head.[0] in
      let distance =
        int_of_string (String.sub head 1 (String.length head - 1))
      in
      let new_dial, _ =
        positive_mod ((get_operator rotation) dial distance) 100
      in
      (* keep track of the number of times we point to 0 *)
      if new_dial = 0 then part_1 tail new_dial (num_zeroes + 1)
      else part_1 tail new_dial num_zeroes
  | [] -> num_zeroes

let () =
  print_endline
    ("Part 1: "
    ^ string_of_int
        (* the dial starts pointing at 50 *)
        (part_1 (In_channel.input_lines (open_in "resources/day1.txt")) 50 0))

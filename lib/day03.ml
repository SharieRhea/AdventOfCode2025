(* scan the string from left to right beginning at index start
 * return the highest digit found and the FIRST index it was found at
 * but stop short of the end of the string based on how many digits
 * are left to find
 *)
let find_highest_digit bank start digit total_digits =
  let rec scan current_index max index_of_max =
    let length = String.length bank in
    (* we cannot go further because there are too many remaining digits *)
    if current_index = length - (total_digits - digit) then (max, index_of_max)
    else if current_index >= length then (max, index_of_max)
    else
      let digit = bank.[current_index] in
      (* 9 is the highest it can be anyway, don't check any further *)
      if digit = '9' then ('9', current_index)
      else if digit > max then scan (current_index + 1) digit current_index
      else scan (current_index + 1) max index_of_max
  in
  scan start '0' (-1)

let rec get_joltage battery_banks sum digits =
  match battery_banks with
  | head :: tail ->
      (* this is a really scuffed for loop lol *)
      let rec traverse build start iteration =
        if iteration > digits then List.rev build
        else
          let digit, index = find_highest_digit head start iteration digits in
          traverse (digit :: build) (index + 1) (iteration + 1)
      in
      let jolts =
        traverse [] 0 1 |> List.to_seq |> String.of_seq |> int_of_string
      in
      get_joltage tail (sum + jolts) digits
  | [] -> sum

let run () =
  print_endline "DAY 3:";
  let battery_banks = In_channel.input_lines @@ open_in "resources/day03.txt" in
  print_endline ("    Part 1: " ^ string_of_int @@ get_joltage battery_banks 0 2);
  print_endline
    ("    Part 2: " ^ string_of_int @@ get_joltage battery_banks 0 12)

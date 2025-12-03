open AdventOfCode2025

let () =
  print_endline "Enter a day to run or 0 for all days: ";
  let day = read_int () in
  match day with
  | 0 ->
      Day1.run ();
      Day2.run ()
  | 1 -> Day1.run ()
  | 2 -> Day2.run ()
  | _ -> print_endline "Invalid entry"

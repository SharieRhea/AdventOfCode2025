open AdventOfCode2025

let () =
  print_endline "Enter a day to run or 0 for all days: ";
  let day = read_int () in
  match day with
  | 0 ->
      Day01.run ();
      Day02.run ();
      Day03.run ();
      Day04.run ();
      Day05.run ()
  | 1 -> Day01.run ()
  | 2 -> Day02.run ()
  | 3 -> Day03.run ()
  | 4 -> Day04.run ()
  | 5 -> Day05.run ()
  | _ -> print_endline "Invalid entry"

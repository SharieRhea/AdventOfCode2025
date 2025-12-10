open AdventOfCode2025

let days =
  [
    Day01.run;
    Day02.run;
    Day03.run;
    Day04.run;
    Day05.run;
    Day06.run;
    Day07.run;
    Day08.run;
    Day09.run;
  ]

let () =
  print_endline "Enter a day to run or 0 for all days: ";
  let input = read_int () in
  match input with
  | 0 -> List.iter (fun day -> day ()) days
  | _ -> (
      match List.nth_opt days (input - 1) with
      | Some day -> day ()
      | None -> failwith "Invalid day!")

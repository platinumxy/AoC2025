let days =
  [|
    (Day1.get_input, (Day1.part1, Day1.part2));
    (Day2.get_input, (Day2.part1, Day2.part2));
    (Day3.get_input, (Day3.part1, Day3.part2));
  |]

let run_day idx =
  if idx < 1 || idx > Array.length days then
    Printf.printf "Day %d not implemented yet\n" idx
  else
    let input, (pt1, pt2) = days.(idx - 1) in
    Printf.printf "Day %d\tPart 1: %d\n" idx (pt1 input);
    Printf.printf "Day %d\tPart 2: %d\n" idx (pt2 input)

let run_all () = Array.iteri (fun i _ -> run_day (i + 1)) days

let current_day () =
  let tm = Unix.localtime (Unix.time ()) in
  if tm.Unix.tm_mon = 11 then Some tm.Unix.tm_mday else None

let parse_args () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | [ "--all" ] -> `All
  | [ "-d"; n ] | [ "--day"; n ] -> (
      match int_of_string_opt n with
      | Some day -> `Day day
      | None -> failwith ("Invalid day number: " ^ n))
  | [] -> ( match current_day () with Some d -> `Day d | None -> `All)
  | _ -> failwith "Usage: aoc [--all | -d/--day <num>]"

let () = match parse_args () with `All -> run_all () | `Day n -> run_day n

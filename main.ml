open! Days.Registry

let run_day idx =
  match get idx with
  | None -> Printf.printf "Day %d not implemented yet\n" idx
  | Some (Day { get_input; part1; part2 }) ->
      let input = get_input () in
      Printf.printf "Day %d\tPart 1: %d\n" idx (part1 input);
      Printf.printf "Day %d\tPart 2: %d\n" idx (part2 input);
      flush stdout

let run_all () =
  iter (fun n _ -> run_day n)

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

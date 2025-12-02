let _dud (_x : string list) : int = 0

let () =
  let days =
    [
      (Day1.get_input, (Day1.part1, Day1.part2));
      (Day2.get_input, (Day2.part1, Day2.part2));
    ]
  in
  List.iteri
    (fun idx (input, (pt1, pt2)) ->
      Printf.printf "D%dP1: %d\n" (idx + 1) (pt1 input);
      Printf.printf "D%dP2: %d\n" (idx + 1) (pt2 input))
    days

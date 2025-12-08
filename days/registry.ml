type 'a day = { get_input : unit -> 'a; part1 : 'a -> int; part2 : 'a -> int }
type packed_day = Day : 'a day -> packed_day

let days : (int, packed_day) Hashtbl.t = Hashtbl.create 25

let register n ~get_input ~part1 ~part2 =
  Hashtbl.replace days n (Day { get_input; part1; part2 })

let get n = Hashtbl.find_opt days n

let max_day () =
  Hashtbl.fold (fun k _ acc -> max k acc) days 0

let iter f =
  let sorted = Hashtbl.to_seq days |> List.of_seq |> List.sort compare in
  List.iter (fun (n, d) -> f n d) sorted

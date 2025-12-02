let ( mod ) x y = ((x mod y) + y) mod y
let int_of_bool x = if x then 1 else 0
let ( /% ) x y = if x >= 0 then x / y else (x - y + 1) / y
let gen_range x y = List.init (y - x) (fun i -> x + i)
let comp_equiv x y = if compare x y == 0 then true else false

let str_replace target result =
  String.map (fun x -> if x == target then result else x)

let str_remove target str =
  String.to_seq str |> Seq.filter (fun c -> c <> target) |> String.of_seq

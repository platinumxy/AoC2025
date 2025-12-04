let ( mod ) x y = ((x mod y) + y) mod y
let int_of_bool x = if x then 1 else 0
let bool_of_int x = if x != 0 then true else false
let ( /% ) x y = if x >= 0 then x / y else (x - y + 1) / y
let gen_range x y = List.init (y - x) (fun i -> x + i)
let comp_equiv x y = if compare x y == 0 then true else false

let str_replace target result =
  String.map (fun x -> if x == target then result else x)

let str_remove target str =
  String.to_seq str |> Seq.filter (fun c -> c <> target) |> String.of_seq

let char_list_of_string str = String.to_seq str |> List.of_seq
let zip l1 l2 = List.map2 (fun a b -> (a, b)) l1 l2
let enumerate l = zip (gen_range 0 (List.length l)) l

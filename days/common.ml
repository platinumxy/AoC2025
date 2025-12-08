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
let string_of_char_list cl = cl |> List.to_seq |> String.of_seq
let zip l1 l2 = List.map2 (fun a b -> (a, b)) l1 l2
let enumerate l = zip (gen_range 0 (List.length l)) l

let split_at idx =
  let rec aux acc n = function
    | [] -> (List.rev acc, [])
    | h :: t ->
        if n == 0 then (List.rev acc, h :: t) else aux (h :: acc) (n - 1) t
  in
  aux [] idx

let sum = List.fold_left ( + ) 0

let transpose mtrx =
  let rec aux acc = function
    | [] | [] :: _ -> List.rev acc
    | rows ->
        let heads = List.map List.hd rows in
        let tails = List.map List.tl rows in
        aux (heads :: acc) tails
  in
  aux [] mtrx

let strip str =
  let rec remove_double_spaces s =
    let rec aux acc prev = function
      | [] -> String.of_seq (List.to_seq (List.rev acc))
      | h :: t ->
          if h = ' ' && prev = ' ' then aux acc prev t else aux (h :: acc) h t
    in
    let s' = aux [] '\000' (List.of_seq (String.to_seq s)) in
    if s' = s then s else remove_double_spaces s'
  in
  str |> String.trim |> str_remove '\r' |> str_remove '\n'
  |> remove_double_spaces

let last lst =
  match List.rev lst with [] -> failwith "Empty list" | h :: _ -> h

let get_input () =
  In_channel.with_open_text "inputs/day3.input" In_channel.input_lines

let rec merge = function [] -> "" | x :: xs -> string_of_int x ^ merge xs

let rec atomoise = function
  | a :: rest -> (int_of_char a - int_of_char '0') :: atomoise rest
  | [] -> []

let rec get_max = function
  | [] -> (-1, [])
  | x :: xs ->
      let mx, rem = get_max xs in
      if x >= mx then (x, xs) else (mx, rem)

let without_last ?(n = 1) lst =
  let len = List.length lst in
  if n >= len then (lst, [])
  else
    let rec aux acc count = function
      | [] -> (List.rev acc, [])
      | x :: xs ->
          if count < len - n then aux (x :: acc) (count + 1) xs
          else (List.rev acc, x :: xs)
    in
    aux [] 0 lst

let max_for n vals =
  let rec aux count = function
    | [] -> failwith "List shorter than n"
    | x :: xs ->
        let usable, unusable = without_last ~n:(n - count) (x :: xs) in
        let mx, dropped = get_max usable in
        mx :: (if count == n then [] else aux (count + 1) (dropped @ unusable))
  in
  merge (aux 1 vals)

let solve_with fn lines =
  List.to_seq lines
  |> Seq.map (fun s -> String.to_seq s |> List.of_seq)
  |> Seq.map atomoise |> Seq.map fn |> Seq.map int_of_string
  |> Seq.fold_left ( + ) 0
  |> fun x -> x

let part1 = solve_with (max_for 2)
let part2 = solve_with (max_for 12)

type range = { start : int; end_ : int }

let parse_range str =
  match String.split_on_char '-' str with
  | [ start; end_ ] -> (
      match (int_of_string_opt start, int_of_string_opt end_) with
      | Some s, Some e -> { start = s; end_ = e }
      | _ -> failwith ("Invalid range: " ^ str))
  | _ -> failwith ("Invalid range format: " ^ str)

let parse_ranges strs =
  strs |> List.map parse_range
  |> List.sort (fun r1 r2 -> compare r1.start r2.start)

let get_input () =
  let lines =
    In_channel.with_open_text "inputs/day5.input" In_channel.input_lines
  in
  match List.find_index (Common.comp_equiv "") lines with
  | None -> failwith "Invalid input format"
  | Some split_pt -> (
      let ranges, rest = Common.split_at split_pt lines in
      match rest with
      | _ :: items -> (parse_ranges ranges, List.map int_of_string items)
      | [] -> failwith "Invalid input format: missing items after split")

let in_range item rng = item >= rng.start && item <= rng.end_

let part1 (ranges, items) =
  let is_fresh item = List.exists (in_range item) ranges in
  items |> List.filter is_fresh |> List.length

let part2 (ranges, _) =
  let rec find_cap curr = function
    | [] -> (curr.end_, [])
    | r2 :: rem ->
        if curr.end_ < r2.start then (curr.end_, r2 :: rem)
        else find_cap (if r2.end_ > curr.end_ then r2 else curr) rem
  in
  let rec fold_overlaps = function
    | [] -> []
    | r1 :: rngs ->
        let end_, remaining = find_cap r1 rngs in
        { start = r1.start; end_ } :: fold_overlaps remaining
  in
  let range_size rng = rng.end_ - rng.start + 1 in
  ranges |> fold_overlaps |> List.map range_size |> Common.sum

let () = Registry.register 5 ~get_input ~part1 ~part2

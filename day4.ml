let get_input () =
  List.map Common.char_list_of_string
    (In_channel.with_open_text "inputs/day4.input" In_channel.input_lines)

let find map idx jdx =
  if idx < 0 || jdx < 0 then 0
  else
    match List.nth_opt map idx with
    | None -> 0
    | Some row -> (
        match List.nth_opt row jdx with
        | Some v -> Common.int_of_bool (v == '@')
        | None -> 0)

let count_around map idx jdx =
  let mapped_find = find map in
  let deltas =
    [| (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) |]
  in
  Array.fold_left
    (fun acc (dx, dy) -> acc + mapped_find (idx + dx) (jdx + dy))
    0 deltas

let zip_cords map =
  List.mapi (fun i row -> List.mapi (fun j v -> (v, i, j)) row) map

let reconstruct_map (tagged_map : (int * char * int * int) list) =
  let rec aux acc curr_row = function
    | [] -> List.rev (List.rev curr_row :: acc)
    | (tagged, v, _, j) :: rest ->
        let cell = if Common.bool_of_int tagged then '.' else v in
        if j == 0 then aux (List.rev curr_row :: acc) [ cell ] rest
        else aux acc (cell :: curr_row) rest
  in
  aux [] [] tagged_map

let tag_map map =
  let resolve_cell (v, idx, jdx) =
    (Common.int_of_bool (v == '@' && count_around map idx jdx < 4), v, idx, jdx)
  in
  List.map resolve_cell (List.flatten (zip_cords map))

let count_tagged tagged_map =
  List.fold_left (fun acc (count, _, _, _) -> count + acc) 0 tagged_map

let part1 map = count_tagged (tag_map map)

let part2 map =
  let rec aux acc curr_map =
    let tagged = tag_map curr_map in
    let count = count_tagged tagged in
    let new_map = reconstruct_map tagged in
    if Common.comp_equiv new_map curr_map then count + acc
    else aux (acc + count) new_map
  in
  aux 0 map

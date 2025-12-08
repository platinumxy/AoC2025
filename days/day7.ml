let get_input () =
  In_channel.with_open_text "inputs/day7.input" In_channel.input_lines
  |> List.map Common.char_list_of_string

let find_start diagram =
  match List.find_index (( = ) 'S') (List.hd diagram) with
  | None -> failwith "No starting point found"
  | Some idx -> idx

let part1 diagram =
  let resolve_splits row incoming_lasers =
    let process (lasers, splits, cnt) (idx, c) =
      match c with
      | '.' -> (lasers, splits, cnt)
      | '^' when List.mem idx incoming_lasers ->
          ((idx - 1) :: (idx + 1) :: lasers, idx :: splits, cnt + 1)
      | '^' -> (lasers, splits, cnt)
      | _ -> failwith "Invalid character in diagram"
    in
    let lasers, splits, cnt =
      List.fold_left process ([], [], 0) (Common.enumerate row)
    in
    let not_split =
      List.filter (fun idx -> not (List.mem idx splits)) incoming_lasers
    in
    (List.sort_uniq compare (lasers @ not_split), cnt)
  in
  let rec aux total_splits lasers = function
    | [] -> total_splits
    | row :: rows ->
        let new_lasers, splits = resolve_splits row lasers in
        aux (total_splits + splits) new_lasers rows
  in
  aux 0 [ find_start diagram ] (List.tl diagram)

let part2 diagram =
  let add_to acc p count =
    match List.assoc_opt p acc with
    | Some n -> (p, n + count) :: List.remove_assoc p acc
    | None -> (p, count) :: acc
  in
  let resolve_splits row incoming =
    let process acc (pos, count) =
      if List.nth row pos = '^' then
        add_to (add_to acc (pos - 1) count) (pos + 1) count
      else add_to acc pos count
    in
    List.fold_left process [] incoming
  in
  let rec aux timelines = function
    | [] -> List.fold_left (fun acc (_, count) -> acc + count) 0 timelines
    | row :: rows -> aux (resolve_splits row timelines) rows
  in
  aux [ (find_start diagram, 1) ] (List.tl diagram)

let () = Registry.register 7 ~get_input ~part1 ~part2

type eq = { vals : int list; op : char }

let get_input () =
  In_channel.with_open_text "inputs/day6.input" In_channel.input_lines

let resolve_eq eq =
  match eq.op with
  | '+' -> List.fold_left ( + ) 0 eq.vals
  | '*' -> List.fold_left ( * ) 1 eq.vals
  | _ -> failwith ("Unknown operator: " ^ String.make 1 eq.op)

let part1 lines =
  let get_postfix lines =
    let split_lines =
      List.map (fun s -> String.split_on_char ' ' (Common.strip s)) lines
    in
    let postfix = Common.transpose split_lines in
    List.map
      (fun args ->
        let op_ = Common.last args in
        let vals = List.rev (List.tl (List.rev args)) in
        let vals_int = List.map int_of_string vals in
        { vals = vals_int; op = String.get op_ 0 })
      postfix
  in

  Common.sum (List.map resolve_eq (get_postfix lines))

let part2 lines =
  let rec count_spaces = function ' ' :: t -> 1 + count_spaces t | _ -> 0 in
  let rec extract_blocks acc num_strs = function
    | [] -> List.rev acc
    | op_str ->
        let spaces = count_spaces op_str in
        let width =
          max 1
            (spaces + 1
            + count_spaces (snd (Common.split_at (spaces + 1) op_str)))
        in
        let _, rem_ops = Common.split_at width op_str in
        let numbers, rem_nums =
          List.split (List.map (Common.split_at width) num_strs)
        in
        let parse s =
          let t = Common.strip (Common.string_of_char_list s) in
          if t = "" then None else Some (int_of_string t)
        in
        let vals_ = List.filter_map parse (Common.transpose numbers) in
        let op_char = List.nth op_str spaces in
        extract_blocks ({ vals = vals_; op = op_char } :: acc) rem_nums rem_ops
  in
  let char_lines = List.map Common.char_list_of_string lines in
  let op_line = Common.last char_lines in
  let num_lines = List.rev (List.tl (List.rev char_lines)) in
  Common.sum (List.map resolve_eq (extract_blocks [] num_lines op_line))

let () = Registry.register 6 ~get_input ~part1 ~part2

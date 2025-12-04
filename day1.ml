open Common

let get_input () =
  In_channel.with_open_text "inputs/day1.input" In_channel.input_lines

let conv x =
  let v = int_of_string (String.sub x 1 (String.length x - 1)) in
  match x.[0] with 'R' -> v | 'L' -> -v | _ -> failwith "Invalid input"

let part1 lines =
  let _dial, num_zeros =
    List.fold_left
      (fun (dial, num_zeros) x ->
        let new_dial = (dial + conv x) mod 100 in
        (new_dial, num_zeros + if new_dial = 0 then 1 else 0))
      (50, 0) lines
  in
  num_zeros

let part2 lines =
  let _dial, num_zeros =
    List.fold_left
      (fun (dial, num_zeros) x ->
        let offset = conv x in
        let new_pos = dial + offset in
        let crossings =
          if offset >= 0 then (new_pos /% 100) - (dial /% 100)
          else ((dial - 1) /% 100) - ((new_pos - 1) /% 100)
        in
        (new_pos mod 100, num_zeros + crossings))
      (50, 0) lines
  in
  num_zeros

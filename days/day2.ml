open Common

let get_input () =
  String.split_on_char ','
    (str_remove '\n'
       (In_channel.with_open_text "inputs/day2.input" In_channel.input_all))

let split_string_at str idx =
  (String.sub str 0 idx, String.sub str idx (String.length str - idx))

let extract_range range =
  match String.split_on_char '-' range with
  | [ a; b ] -> (int_of_string a, int_of_string b)
  | _ -> failwith ("Invalid range format: " ^ range)

(* break string into an array of n substrings *)
let atomize_string str sub_arrs =
  let len = String.length str in
  let idxs =
    List.append (List.init sub_arrs (fun i -> i * (len / sub_arrs))) [ len ]
  in
  let rec pairs ?(acc = []) = function
    (* gen all the substr idxs *)
    | a :: b :: rest -> pairs ~acc:((a, b) :: acc) (b :: rest)
    | _ -> List.rev acc
  in
  List.map
    (fun (start, stop) -> String.sub str start (stop - start))
    (pairs idxs)

let invalidIdPT1 id =
  let id_str = string_of_int id in
  let lft, rht = split_string_at id_str (String.length id_str /% 2) in
  if comp_equiv lft rht then id else 0

let invalidIdPT2 id =
  let rec equiv = function
    | a :: b :: rest -> comp_equiv a b && equiv (b :: rest)
    | _ -> true
  in
  let id_str = string_of_int id in
  let len = String.length id_str in
  if len < 2 then 0
  else if len == 2 then if id mod 11 == 0 then id else 0
  else
    let atom_lens = gen_range 2 (len + 1) in

    let atoms = List.map (atomize_string id_str) atom_lens in
    (* see if any atoms are invalid *)
    if List.fold_left ( || ) false (List.map equiv atoms) then id else 0

let validate_input fn input =
  List.fold_left ( + ) 0
    (List.flatten
       (List.map
          (fun range ->
            if String.get range 0 == '0' then [ 0 ]
            else
              let min, maxi = extract_range range in
              List.map fn (gen_range min (maxi + 1)))
          input))

let part1 = validate_input invalidIdPT1
let part2 = validate_input invalidIdPT2

let () = Registry.register 2 ~get_input ~part1 ~part2

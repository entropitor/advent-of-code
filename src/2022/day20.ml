let ( >> ) f g x = f x |> g
let const x y = x
let id x = x

module SeqExt = struct
  let hd seq =
    match seq () with
    | Seq.Nil -> failwith "Nothing in sequence"
    | Seq.Cons (hd, tl) -> hd

  let indexed seq = seq |> Seq.mapi (fun i x -> (i, x))
end

let lines = "1\n2\n-3\n3\n-2\n0\n4" |> String.split_on_char '\n' |> List.to_seq

let input_lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

let parse lines = lines |> Seq.map int_of_string |> List.of_seq

let mix nb_times_to_mix numbers =
  let nb_numbers = List.length numbers in
  let rec find_next with_index acc n =
    match with_index with
    | [] -> None
    | (x, i) :: tl when i = n -> Some (acc, x, tl)
    | hd :: tl -> find_next tl (hd :: acc) n
  in
  let rec skip nb_to_skip rev_hd_list tl_list =
    if nb_to_skip = 0 then (rev_hd_list, tl_list)
    else if nb_to_skip < 0 then
      match rev_hd_list with
      | [] -> skip nb_to_skip (List.rev tl_list) []
      | new_head_of_tail :: rest_of_head ->
          skip (nb_to_skip + 1) rest_of_head (new_head_of_tail :: tl_list)
    else
      match tl_list with
      | [] -> skip nb_to_skip [] (List.rev rev_hd_list)
      | hd :: tl -> skip (nb_to_skip - 1) (hd :: rev_hd_list) tl
  in
  let mix_one with_index =
    let rec f with_index n =
      if n = List.length numbers then with_index
      else
        match find_next with_index [] n with
        | None -> failwith "Should not happen"
        | Some (rev_hd_list, next, tl_list) ->
            let rev_hd_list, tl_list =
              let nb_to_skip = next mod (nb_numbers - 1) in
              skip nb_to_skip rev_hd_list tl_list
            in
            let new_tail = (next, n) :: tl_list in
            f (List.rev_append rev_hd_list new_tail) (n + 1)
    in
    f with_index 0
  in
  let with_index = numbers |> List.mapi (fun i x -> (x, i)) in
  let rec mix with_index nb_times_to_mix =
    if nb_times_to_mix = 0 then with_index
    else mix (mix_one with_index) (nb_times_to_mix - 1)
  in
  mix with_index nb_times_to_mix |> List.map fst |> Array.of_list

let solve mixed =
  let index_of_zero =
    mixed |> Array.to_seqi
    |> Seq.find_map (fun (i, x) -> if x = 0 then Some i else None)
    |> Option.get
  in
  let nb_after_zero n =
    let length = Array.length mixed in
    mixed.((n + index_of_zero) mod length)
  in
  Printf.printf "1000=%d\n2000=%d\n3000=%d\n" (nb_after_zero 1000)
    (nb_after_zero 2000) (nb_after_zero 3000);
  nb_after_zero 1000 + nb_after_zero 2000 + nb_after_zero 3000

let solve1 lines =
  let numbers = parse lines in
  mix 1 numbers |> solve

let solve2 lines =
  let numbers = parse lines |> List.map (fun x -> x * 811589153) in
  mix 10 numbers |> solve
;;

parse lines |> mix 1;;
Printf.printf "Part 1 (example): %d\n" (solve1 lines);;
flush stdout;;
Printf.printf "Part 1: %d\n\n" (solve1 input_lines);;
flush stdout;;
Printf.printf "Part 2 (example): %d\n" (solve2 lines);;
flush stdout;;
Printf.printf "Part 2: %d\n" (solve2 input_lines);;
flush stdout

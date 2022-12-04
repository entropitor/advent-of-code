(* let lines = *)
(* "vJrwpWtwJgWrhcsFMMfFFhFp\n\ *)
   (*       jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\ *)
   (*       PmmdzqPrVvPwwTWBwg\n\ *)
   (*       wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\ *)
   (*       ttgJtRGJQctTZtZT\n\ *)
   (*       CrZsJsPPZsGzwwsLwLmpwMDw" |> String.split_on_char '\n' |> List.to_seq *)
let lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

module CharSet = Set.Make (Char)

let set_of_str str = str |> String.to_seq |> CharSet.of_seq
let head_of_seq seq = seq |> Seq.uncons |> Option.get |> fst

let score char =
  if Char.lowercase_ascii char == char then Char.code char - 96
  else Char.code char - 38

let mismatch =
  lines
  |> Seq.map (fun line ->
         let len = String.length line / 2 in
         let first = String.sub line 0 len |> set_of_str in
         let second = String.sub line len len |> set_of_str in
         let shared = CharSet.inter first second in
         shared |> CharSet.to_seq |> head_of_seq)
  |> Seq.map score |> Seq.fold_left ( + ) 0

let part1 = mismatch;;

Printf.printf "part1: %d\n" part1

let groups =
  lines
  |> Seq.mapi (fun i x -> (x, i))
  |> Seq.group (fun a b -> snd b mod 3 != 0)
  |> Seq.map (Seq.map fst)
  |> Seq.map (fun group ->
         let sets = group |> Seq.map set_of_str in
         let shared = sets |> Seq.fold_left CharSet.inter (head_of_seq sets) in
         shared |> CharSet.to_seq |> head_of_seq)
  |> Seq.map score |> Seq.fold_left ( + ) 0

let part2 = groups;;

Printf.printf "part2: %d" part2

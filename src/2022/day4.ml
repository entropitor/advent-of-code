let example_lines =
  "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"
  |> String.split_on_char '\n' |> List.to_seq

let lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

let rec fully_overlaps a b c d =
  if a == c then true
  else if a > c then fully_overlaps c d a b
  else c <= b && d <= b

let do_sections_fully_overlap lines =
  lines |> Seq.map (fun line -> Scanf.sscanf line "%d-%d,%d-%d" fully_overlaps)
;;

do_sections_fully_overlap example_lines |> List.of_seq

let count seq =
  seq
  |> Seq.map (fun overlaps -> if overlaps then 1 else 0)
  |> Seq.fold_left ( + ) 0
;;

example_lines |> do_sections_fully_overlap |> count
|> Printf.printf "Part 1 (example): %d\n"
;;

lines |> do_sections_fully_overlap |> count |> Printf.printf "Part 1: %d\n"

(* ----------------- *)
let rec overlaps a b c d =
  if a == c then true else if a > c then overlaps c d a b else c <= b

let do_sections_overlap lines =
  lines |> Seq.map (fun line -> Scanf.sscanf line "%d-%d,%d-%d" overlaps)
;;

do_sections_overlap example_lines |> List.of_seq;;

example_lines |> do_sections_overlap |> count
|> Printf.printf "Part 2 (example): %d\n"
;;

lines |> do_sections_overlap |> count |> Printf.printf "Part 2: %d\n"

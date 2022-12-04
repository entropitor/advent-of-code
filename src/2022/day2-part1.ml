let lines = Seq.of_dispenser (fun _ -> In_channel.input_line stdin)

(* let lines = "A Y\nB X\nC Z" |> String.split_on_char '\n' |> List.to_seq *)
let score_us b = match b with "X" -> 1 | "Y" -> 2 | "Z" -> 3 | _ -> 0

let score_win a b =
  match (a, b) with
  | "A", "Y" -> 6
  | "B", "Z" -> 6
  | "C", "X" -> 6
  | "A", "X" -> 3
  | "B", "Y" -> 3
  | "C", "Z" -> 3
  | _ -> 0

let scores =
  lines
  |> Seq.map (fun line -> Scanf.sscanf line "%s %s" (fun a b -> (a, b)))
  |> Seq.map (fun (a, b) -> score_win a b + score_us b)

let part1 = scores |> Seq.fold_left (fun x y -> x + y) 0;;

Printf.printf "%d" part1

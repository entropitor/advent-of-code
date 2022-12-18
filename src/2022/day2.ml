let lines = "A Y\nB X\nC Z" |> String.split_on_char '\n' |> List.to_seq

module Part1 = struct
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
end

let part1 = Part1.scores |> Seq.fold_left (fun x y -> x + y) 0;;

Printf.printf "%d" part1

module Part2 = struct
  let score_win b = match b with "X" -> 0 | "Y" -> 3 | "Z" -> 6 | _ -> 0

  let score_us a b =
    match (a, b) with
    | "A", "X" -> 3
    | "B", "X" -> 1
    | "C", "X" -> 2
    (* -- *)
    | "A", "Y" -> 1
    | "B", "Y" -> 2
    | "C", "Y" -> 3
    (* -- *)
    | "A", "Z" -> 2
    | "B", "Z" -> 3
    | "C", "Z" -> 1
    | _ -> 0

  let scores =
    lines
    |> Seq.map (fun line -> Scanf.sscanf line "%s %s" (fun a b -> (a, b)))
    |> Seq.map (fun (a, b) -> score_us a b + score_win b)
end

let part2 = Part2.scores |> Seq.fold_left (fun x y -> x + y) 0;;

Printf.printf "%d" part2

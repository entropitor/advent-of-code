let lines =
  "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2" |> String.split_on_char '\n'
  |> List.to_seq

let input_lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

type command = Right | Left | Up | Down

let parse lines =
  lines
  |> Seq.flat_map (fun line ->
         Scanf.sscanf line "%c %d" (fun chr n ->
             Seq.init n (fun _ ->
                 match chr with
                 | 'R' -> Right
                 | 'L' -> Left
                 | 'D' -> Down
                 | 'U' -> Up
                 | _ -> failwith "Unknown direction")))

let sign x = if x > 0 then 1 else -1

let update_tail (xHead, yHead) (xTail, yTail) =
  match (xHead - xTail, yHead - yTail) with
  | 0, 0 -> (xTail, yTail)
  | 0, 1 -> (xTail, yTail)
  | 1, 0 -> (xTail, yTail)
  | 0, -1 -> (xTail, yTail)
  | -1, 0 -> (xTail, yTail)
  | 1, 1 -> (xTail, yTail)
  | 1, -1 -> (xTail, yTail)
  | -1, -1 -> (xTail, yTail)
  | -1, 1 -> (xTail, yTail)
  (* ---- *)
  | 2, 0 -> (xTail + 1, yTail)
  | -2, 0 -> (xTail - 1, yTail)
  | 0, 2 -> (xTail, yTail + 1)
  | 0, -2 -> (xTail, yTail - 1)
  (* ---- *)
  | x, 0 -> failwith (Printf.sprintf "Unknown difference x=%d y=%d" x 0)
  | 0, y -> failwith (Printf.sprintf "Unknown difference x=%d y=%d" 0 y)
  | xDiff, yDiff -> (xTail + sign xDiff, yTail + sign yDiff)
(* | 2, 1 | 1, 2 -> (xTail + 1, yTail + 1) *)
(* | 2, -1 | 1, -2 -> (xTail + 1, yTail - 1) *)
(* | -2, -1 | -1, -2 -> (xTail - 1, yTail - 1) *)
(* | -2, 1 | -1, 2 -> (xTail - 1, yTail + 1) *)
(* ---- *)

module TupleSet = Set.Make (struct
  type t = int * int

  let compare (a, b) (c, d) =
    let fst = compare a c in
    if fst = 0 then compare b d else fst
end)

let solve1 lines =
  let commands = parse lines in
  commands
  |> Seq.scan
       (fun ((xHead, yHead), tail) command ->
         let head =
           match command with
           | Right -> (xHead + 1, yHead)
           | Left -> (xHead - 1, yHead)
           | Up -> (xHead, yHead + 1)
           | Down -> (xHead, yHead - 1)
         in
         (head, update_tail head tail))
       ((0, 0), (0, 0))
  |> Seq.map snd |> TupleSet.of_seq |> TupleSet.cardinal
;;

parse lines |> List.of_seq;;
solve1 lines;;
Printf.printf "Part 1: %d\n" (solve1 input_lines)

(* ------- *)
let lines2 =
  "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20" |> String.split_on_char '\n'
  |> List.to_seq

let solve2 lines length_of_tail =
  let commands = parse lines in
  commands
  |> Seq.scan
       (fun [@warning "-8"] ((xHead, yHead) :: tail) command ->
         let head =
           match command with
           | Right -> (xHead + 1, yHead)
           | Left -> (xHead - 1, yHead)
           | Up -> (xHead, yHead + 1)
           | Down -> (xHead, yHead - 1)
         in
         tail |> List.to_seq |> Seq.scan update_tail head |> List.of_seq)
       (List.init length_of_tail (fun _ -> (0, 0)))
  |> Seq.map (fun lst -> Array.get (Array.of_list lst) (length_of_tail - 1))
  |> TupleSet.of_seq |> TupleSet.cardinal
;;

(* solve2 lines 3;; *)
(* let arr = solve2 lines2 10 |> Array.of_seq in *)
(* arr.(5 + 8) *)
(* ;; *)

solve2 lines2 10;;
Printf.printf "Part 2: %d\n" (solve2 input_lines 10)

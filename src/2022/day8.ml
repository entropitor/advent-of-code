let lines =
  "30373\n25512\n65332\n33549\n35390" |> String.split_on_char '\n'
  |> List.to_seq

let input_lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

let id x = x
let const x _ = x

let transpose mat =
  let size = Array.length mat in
  let size2 = Array.length mat.(0) in
  let transposed = Array.make size2 (Array.make size 0) in
  for i = 0 to size2 - 1 do
    transposed.(i) <- Array.make size 0;
    for j = 0 to size - 1 do
      transposed.(i).(j) <- mat.(j).(i)
    done
  done;
  transposed

let arr_rev arr =
  let size = Array.length arr in
  let reversed = Array.make size arr.(0) in
  for i = 0 to size - 1 do
    reversed.(i) <- arr.(size - i - 1)
  done;
  reversed

let parse lines =
  lines
  |> Seq.map (fun line ->
         String.to_seq line
         |> Seq.map (fun c -> Scanf.sscanf (String.make 1 c) "%d" id)
         |> Array.of_seq)
  |> Array.of_seq

let solve1 lines =
  let tree_heights = parse lines in
  let size = Array.length tree_heights in
  let max_of heights =
    Seq.init size id
    |> Seq.scan
         (fun acc i ->
           let trees = heights.(i) |> Array.to_seq in
           Seq.zip trees acc |> Seq.map (fun (a, b) -> max a b))
         (Seq.init size (const (-1)))
    |> Seq.take size |> Seq.map Array.of_seq |> Array.of_seq
  in
  let tops = max_of tree_heights in
  let bottoms = max_of (arr_rev tree_heights) |> arr_rev in
  let transposed = transpose tree_heights in
  let left = max_of transposed |> transpose in
  let right = max_of (arr_rev transposed) |> arr_rev |> transpose in
  let visible_trees =
    tree_heights
    |> Array.mapi (fun i row ->
           row
           |> Array.mapi (fun j height ->
                  let visible =
                    tree_heights.(i).(j) > tops.(i).(j)
                    || tree_heights.(i).(j) > bottoms.(i).(j)
                    || tree_heights.(i).(j) > left.(i).(j)
                    || tree_heights.(i).(j) > right.(i).(j)
                  in
                  if visible then 1 else 0))
  in
  ( visible_trees,
    visible_trees
    |> Array.map (Array.fold_left ( + ) 0)
    |> Array.fold_left ( + ) 0 )
;;

solve1 lines;;
Printf.printf "Part 1: %d\n" (solve1 input_lines |> snd)

(* -------------- *)

type height = Smaller | Higher

let solve2 lines =
  let trees = parse lines in
  let size = Array.length trees in
  trees
  |> Array.mapi (fun rowIndex row ->
         row
         |> Array.mapi (fun colIndex height ->
                let count_trees sq =
                  sq
                  |> Seq.fold_left
                       (fun (size, acc) h ->
                         match size with
                         | Smaller ->
                             if h > height then (Higher, acc + 1)
                             else if h == height then (Higher, acc + 1)
                             else (Smaller, acc + 1)
                         | Higher -> (Higher, acc))
                       (Smaller, 0)
                  |> snd
                in
                let top =
                  Seq.init rowIndex (fun i ->
                      trees.(rowIndex - 1 - i).(colIndex))
                  |> count_trees
                in
                let left =
                  Seq.init colIndex (fun i ->
                      trees.(rowIndex).(colIndex - 1 - i))
                  |> count_trees
                in
                let right =
                  Seq.init
                    (size - colIndex - 1)
                    (fun i -> trees.(rowIndex).(colIndex + 1 + i))
                  |> count_trees
                in
                let bottom =
                  Seq.init
                    (size - rowIndex - 1)
                    (fun i -> trees.(rowIndex + 1 + i).(colIndex))
                  |> count_trees
                in
                top * left * right * bottom))
  |> Array.map (Array.fold_left max 0)
  |> Array.fold_left max 0
;;

solve2 lines;;
Printf.printf "Part 2: %d\n" (solve2 input_lines)

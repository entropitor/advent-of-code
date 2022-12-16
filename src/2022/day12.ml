let lines =
  "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"
  |> String.split_on_char '\n' |> List.to_seq

let input_lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

let parse lines =
  let start = ref (0, 0) in
  let goal = ref (0, 0) in
  let matrix =
    lines
    |> Seq.mapi (fun row line ->
           line |> String.to_seq
           |> Seq.mapi (fun col c ->
                  let elevation =
                    match c with
                    | 'S' ->
                        start := (row, col);
                        'a'
                    | 'E' ->
                        goal := (row, col);
                        'z'
                    | _ -> c
                  in
                  Char.code elevation - 97)
           |> Array.of_seq)
    |> Array.of_seq
  in
  (matrix, !start, !goal)

let id x = x
let const x y = x

let shortest_path (matrix, start, goal) =
  let nb_rows = Array.length matrix in
  let nb_cols = Array.length matrix.(0) in
  let nb_elems = nb_rows * nb_cols in
  let matrix =
    matrix |> Array.to_seq |> Seq.flat_map Array.to_seq |> Array.of_seq
  in
  let queue = ref (List.init nb_elems id) in
  let number_of (row, col) = (row * nb_cols) + col in
  let start = number_of start in
  let goal = number_of goal in
  Printf.printf "Start: %d, Goal: %d, Size: %d\n" start goal nb_elems;
  let dist = Array.init nb_elems (const Int.max_int) in
  let prev = Array.init nb_elems (const None) in
  let found = ref false in
  Printf.printf "Finding shortest route...\n";
  flush stdout;
  dist.(start) <- 0;
  while List.length !queue > 0 do
    let[@warning "-8"] (u :: tl) =
      List.stable_sort (fun a b -> compare dist.(a) dist.(b)) !queue
    in
    queue := tl;
    if u = goal || !found then (
      found := true;
      queue := [])
    else
      let v = u - nb_cols in
      if v >= 0 && matrix.(v) <= matrix.(u) + 1 && dist.(u) + 1 < dist.(v) then (
        dist.(v) <- dist.(u) + 1;
        prev.(v) <- Some u);
      let v = u + nb_cols in
      if v < nb_elems && matrix.(v) <= matrix.(u) + 1 && dist.(u) + 1 < dist.(v)
      then (
        dist.(v) <- dist.(u) + 1;
        prev.(v) <- Some u);
      let v = u + 1 in
      if
        v < nb_elems
        && v / nb_cols = u / nb_cols
        && matrix.(v) <= matrix.(u) + 1
        && dist.(u) + 1 < dist.(v)
      then (
        dist.(v) <- dist.(u) + 1;
        prev.(v) <- Some u);
      let v = u - 1 in
      if
        v >= 0
        && v / nb_cols = u / nb_cols
        && matrix.(v) <= matrix.(u) + 1
        && dist.(u) + 1 < dist.(v)
      then (
        dist.(v) <- dist.(u) + 1;
        prev.(v) <- Some u);
      ()
  done;
  Printf.printf "Calculating path...\n";
  flush stdout;
  let rec from node (_ : unit) =
    Seq.(
      if node = start then Cons (node, fun _ -> Nil)
      else
        match prev.(node) with
        | None -> Nil
        | Some other -> Cons (node, from other))
  in
  let nodes = from goal in
  nodes

let solve1 lines = (parse lines |> shortest_path |> Seq.length) - 1;;

parse lines |> shortest_path |> Seq.length;;
solve1 lines;;
Printf.printf "Part 1: %d\n" (solve1 input_lines)

(* ---- *)

let shortest_path_rev (matrix, start, goal) =
  let nb_rows = Array.length matrix in
  let nb_cols = Array.length matrix.(0) in
  let nb_elems = nb_rows * nb_cols in
  let matrix =
    matrix |> Array.to_seq |> Seq.flat_map Array.to_seq |> Array.of_seq
  in
  let queue = ref (List.init nb_elems id) in
  let number_of (row, col) = (row * nb_cols) + col in
  let start = number_of start in
  let goal = number_of goal in
  Printf.printf "Start: %d, Goal: %d, Size: %d\n" start goal nb_elems;
  let dist = Array.init nb_elems (const Int.max_int) in
  let prev = Array.init nb_elems (const None) in
  let found = ref None in
  Printf.printf "Finding shortest route...\n";
  flush stdout;
  dist.(goal) <- 0;
  while List.length !queue > 0 do
    let[@warning "-8"] (u :: tl) =
      List.stable_sort (fun a b -> compare dist.(a) dist.(b)) !queue
    in
    queue := tl;
    if matrix.(u) = 0 then (
      found := Some u;
      queue := [])
    else
      let v = u - nb_cols in
      if v >= 0 && matrix.(u) <= matrix.(v) + 1 && dist.(u) + 1 < dist.(v) then (
        dist.(v) <- dist.(u) + 1;
        prev.(v) <- Some u);
      let v = u + nb_cols in
      if v < nb_elems && matrix.(u) <= matrix.(v) + 1 && dist.(u) + 1 < dist.(v)
      then (
        dist.(v) <- dist.(u) + 1;
        prev.(v) <- Some u);
      let v = u + 1 in
      if
        v < nb_elems
        && v / nb_cols = u / nb_cols
        && matrix.(u) <= matrix.(v) + 1
        && dist.(u) + 1 < dist.(v)
      then (
        dist.(v) <- dist.(u) + 1;
        prev.(v) <- Some u);
      let v = u - 1 in
      if
        v >= 0
        && v / nb_cols = u / nb_cols
        && matrix.(u) <= matrix.(v) + 1
        && dist.(u) + 1 < dist.(v)
      then (
        dist.(v) <- dist.(u) + 1;
        prev.(v) <- Some u);
      ()
  done;
  Printf.printf "Calculating path...\n";
  flush stdout;
  let rec from node (_ : unit) =
    Seq.(
      if node = goal then Cons (node, fun _ -> Nil)
      else
        match prev.(node) with
        | None -> Nil
        | Some other -> Cons (node, from other))
  in
  let nodes = from (!found |> Option.get) in
  nodes

let solve2 lines = (parse lines |> shortest_path_rev |> Seq.length) - 1;;

parse lines |> shortest_path_rev |> List.of_seq;;
solve2 lines;;
Printf.printf "Part 2: %d\n" (solve2 input_lines)

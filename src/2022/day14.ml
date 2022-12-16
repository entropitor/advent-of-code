(* #load "str.cma" *)

let ( >> ) f g x = f x |> g
let const x y = x
let id x = x

let lines =
  "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"
  |> String.split_on_char '\n' |> List.to_seq

let input_lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

let parse lines =
  lines
  |> Seq.map
       (Str.split (Str.regexp " -> ")
       >> List.to_seq
       >> Seq.map (fun str -> Scanf.sscanf str "%d,%d" (fun x y -> (x, y))))

let solve part1 lines =
  let caves = parse lines in
  let width = 500 * 2 in
  let height = 400 in
  let size = width * height in
  let matrix = Array.init size (const false) in
  let pos x y = (y * width) + x in
  let max_y = ref 0 in
  caves
  |> Seq.iter (fun cave ->
         Seq.iter2
           (fun (x1, y1) (x2, y2) ->
             max_y := max !max_y (max y1 y2);
             if x1 == x2 then
               for y = min y1 y2 to max y1 y2 do
                 matrix.(pos x1 y) <- true
               done
             else if y1 == y2 then
               for x = min x1 x2 to max x1 x2 do
                 matrix.(pos x y1) <- true
               done
             else failwith "diagonal")
           cave
           (cave |> Seq.drop 1));
  if not part1 then
    for x = 0 to width - 1 do
      matrix.(pos x (!max_y + 2)) <- true
    done;
  let free_at x y = not matrix.(pos x y) in
  let drop (_ : unit) =
    let x = ref 500 in
    let y = ref 0 in
    let quit_loop = ref false in
    let free_falling = ref false in
    while (not !quit_loop) && not !free_falling do
      if !y > !max_y + 5 then free_falling := true
      else if free_at !x (!y + 1) then y := !y + 1
      else if free_at (!x - 1) (!y + 1) then (
        x := !x - 1;
        y := !y + 1)
      else if free_at (!x + 1) (!y + 1) then (
        x := !x + 1;
        y := !y + 1)
      else quit_loop := true
    done;
    if !free_falling then None else Some (!x, !y)
  in
  Seq.repeat ()
  |> Seq.map (fun _ ->
         if not (free_at 500 0) then None
         else
           match drop () with
           | None -> None
           | Some (x, y) ->
               matrix.(pos x y) <- true;
               Some (x, y))
  |> Seq.take_while Option.is_some
  |> Seq.length

let solve1 = solve true
let solve2 = solve false;;

Printf.printf "Part 1 (example): %d\n" (solve1 lines);;
Printf.printf "Part 1: %d\n" (solve1 input_lines);;
Printf.printf "Part 2 (example): %d\n" (solve2 lines);;
Printf.printf "Part 2: %d\n" (solve2 input_lines)

let ( >> ) f g x = f x |> g
let const x y = x
let id x = x

let lines =
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\n\
   Sensor at x=9, y=16: closest beacon is at x=10, y=16\n\
   Sensor at x=13, y=2: closest beacon is at x=15, y=3\n\
   Sensor at x=12, y=14: closest beacon is at x=10, y=16\n\
   Sensor at x=10, y=20: closest beacon is at x=10, y=16\n\
   Sensor at x=14, y=17: closest beacon is at x=10, y=16\n\
   Sensor at x=8, y=7: closest beacon is at x=2, y=10\n\
   Sensor at x=2, y=0: closest beacon is at x=2, y=10\n\
   Sensor at x=0, y=11: closest beacon is at x=2, y=10\n\
   Sensor at x=20, y=14: closest beacon is at x=25, y=17\n\
   Sensor at x=17, y=20: closest beacon is at x=21, y=22\n\
   Sensor at x=16, y=7: closest beacon is at x=15, y=3\n\
   Sensor at x=14, y=3: closest beacon is at x=15, y=3\n\
   Sensor at x=20, y=1: closest beacon is at x=15, y=3"
  |> String.split_on_char '\n' |> List.to_seq

let input_lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

let parse lines =
  lines
  |> Seq.map (fun line ->
         Scanf.sscanf line
           "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
           (fun sx sy bx by -> (sx, sy, bx, by)))

let solve1_naive lines y =
  let sensors = parse lines in
  let min_x =
    sensors
    |> Seq.flat_map (fun (sx, _, bx, _) ->
           Seq.empty |> Seq.cons sx |> Seq.cons bx)
    |> Seq.fold_left min Int.max_int
  in
  let max_x =
    sensors
    |> Seq.flat_map (fun (sx, _, bx, _) ->
           Seq.empty |> Seq.cons sx |> Seq.cons bx)
    |> Seq.fold_left max Int.min_int
  in
  let nb_cannot_be = ref 0 in
  let dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2) in
  for x = min_x to max_x do
    Printf.printf "Checking %d\n" x;
    flush stdout;
    let is_closer_to_sensor =
      sensors
      |> Seq.exists (fun (sx, sy, bx, by) ->
             dist (sx, sy) (x, y) <= dist (sx, sy) (bx, by))
    in
    let has_beacon =
      sensors |> Seq.exists (fun (_, _, bx, by) -> bx = x && by = y)
    in
    if is_closer_to_sensor && not has_beacon then
      nb_cannot_be := !nb_cannot_be + 1
  done;
  !nb_cannot_be

(* parse lines |> List.of_seq;; *)
(* solve1_naive lines 10;; *)

module IntSet = Set.Make (struct
  type t = int

  let compare x y = compare x y
end)

let solve1 lines y =
  let sensors = parse lines in
  let dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2) in
  sensors
  |> Seq.flat_map (fun (sx, sy, bx, by) ->
         let distance = dist (sx, sy) (bx, by) in
         let dy = abs (sy - y) in
         let max_dx = distance - dy in
         let unfiltered =
           if max_dx < 0 then Seq.empty
           else
             Seq.init
               ((2 * max_dx) + 1)
               (fun i ->
                 let dx = if i mod 2 == 0 then i / 2 else -(i + 1) / 2 in
                 sx + dx)
         in
         unfiltered |> Seq.filter (fun px -> (bx, by) <> (px, y)))
  |> IntSet.of_seq |> IntSet.cardinal
(* |> Seq.map (IntSet.of_seq >> IntSet.to_seq >> List.of_seq) *)

(* solve1 lines 10 *)

(* Printf.printf "Part 1: %d\n" (solve1 input_lines 2000000) *)

(* ------------ *)

let solve2_naive lines max_xy =
  let sensors = parse lines in
  let dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2) in
  let found = ref None in
  for x = 0 to max_xy do
    if Option.is_none !found then
      for y = 0 to max_xy do
        if Option.is_none !found then (
          if x + (y mod 1000) = 0 then (
            Printf.printf "Checking %d,%d\n" x y;
            flush stdout);
          let is_closer_to_sensor =
            sensors
            |> Seq.exists (fun (sx, sy, bx, by) ->
                   dist (sx, sy) (x, y) <= dist (sx, sy) (bx, by))
          in
          let has_beacon =
            sensors |> Seq.exists (fun (_, _, bx, by) -> bx = x && by = y)
          in
          if (not is_closer_to_sensor) && not has_beacon then
            found := Some (x, y))
      done
  done;
  let x, y = Option.get !found in
  Printf.printf "Found %d,%d\n" x y;
  (x * 4000000) + y

let solve2 lines max_xy =
  Printf.printf "Solving part2\n";
  flush stdout;
  let sensors = parse lines in
  let dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2) in
  let rec merge lst =
    match lst with
    | [] -> []
    | [ hd ] -> [ hd ]
    | (a, b) :: (c, d) :: tl ->
        if c <= b then merge ((a, max b d) :: tl)
        else (a, b) :: merge ((c, d) :: tl)
  in
  let x, y =
    Seq.init max_xy (fun y ->
        if y mod 1000 = 0 then (
          Printf.printf "Checking y=%d\n" y;
          flush stdout);
        let not_in =
          sensors
          |> Seq.flat_map (fun (sx, sy, bx, by) ->
                 let distance = dist (sx, sy) (bx, by) in
                 let dy = abs (sy - y) in
                 let max_dx = distance - dy in
                 if max_dx < 0 then Seq.empty
                 else Seq.empty |> Seq.cons (sx - max_dx, sx + max_dx))
          |> List.of_seq |> List.sort compare
        in
        let merged = merge not_in in
        let x1, x2 = List.hd merged in
        if x1 > 0 then Some (0, y)
        else if x2 < max_xy then Some (x2 + 1, y)
        else None)
    |> Seq.drop_while Option.is_none
    |> Seq.take 1 |> List.of_seq |> List.hd |> Option.get
  in
  Printf.printf "Found %d %d\n" x y;
  (x * 4000000) + y
;;

(* solve2_naive lines 20;; *)
solve2 lines 20;;
Printf.printf "Part 2: %d\n" (solve2 input_lines 4000000)

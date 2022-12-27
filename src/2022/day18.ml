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

let lines =
  "2,2,2\n\
   1,2,2\n\
   3,2,2\n\
   2,1,2\n\
   2,3,2\n\
   2,2,1\n\
   2,2,3\n\
   2,2,4\n\
   2,2,6\n\
   1,2,5\n\
   3,2,5\n\
   2,1,5\n\
   2,3,5" |> String.split_on_char '\n' |> List.to_seq

let input_lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

let parse lines =
  lines
  |> Seq.map (fun line ->
         Scanf.sscanf line "%d,%d,%d" (fun x y z -> (x + 1, y + 1, z + 1)))

let solve lines =
  let coordinates = parse lines |> Seq.memoize in

  let map =
    Array.init 100 (fun _ -> Array.init 100 (fun _ -> Array.make 100 false))
  in

  coordinates |> Seq.iter (fun (x, y, z) -> map.(x).(y).(z) <- true);
  let surfaces =
    coordinates
    |> Seq.flat_map (fun (x, y, z) ->
           [
             (x + 1, y, z);
             (x - 1, y, z);
             (x, y + 1, z);
             (x, y - 1, z);
             (x, y, z + 1);
             (x, y, z - 1);
           ]
           |> List.to_seq)
    |> Seq.filter (fun (x, y, z) -> not map.(x).(y).(z))
  in
  (surfaces, map)

let solve1 lines = solve lines |> fst |> Seq.length

module SurfaceSet = Set.Make (struct
  type t = int * int * int

  let compare = compare
end)

let solve2 lines =
  let surfaces, map = solve lines in

  let unreachable_surfaces = ref SurfaceSet.empty in

  let bfs from =
    if SurfaceSet.mem from !unreachable_surfaces then false
    else
      let queue = Queue.create () in
      Queue.push from queue;
      let visited = ref SurfaceSet.empty in

      let should_visit (x, y, z) =
        not (SurfaceSet.mem (x, y, z) !visited || map.(x).(y).(z))
      in

      let reached = ref false in
      while (not (Queue.is_empty queue)) && not !reached do
        let node = Queue.pop queue in
        let x, y, z = node in

        if x <= 0 || y <= 0 || z <= 0 then reached := true
        else if should_visit node then (
          visited := SurfaceSet.add node !visited;

          [
            (x + 1, y, z);
            (x - 1, y, z);
            (x, y + 1, z);
            (x, y - 1, z);
            (x, y, z + 1);
            (x, y, z - 1);
          ]
          |> List.iter (fun neighbour -> Queue.push neighbour queue))
      done;

      if not !reached then
        unreachable_surfaces :=
          !unreachable_surfaces
          |> SurfaceSet.add_seq (SurfaceSet.to_seq !visited);

      !reached
  in

  surfaces |> Seq.filter (fun surface -> bfs surface) |> Seq.length
;;

Printf.printf "Part 1 (example): %d\n" (solve1 lines);;
Printf.printf "Part 1: %d\n" (solve1 input_lines);;
Printf.printf "Part 2 (example): %d\n" (solve2 lines);;
Printf.printf "Part 2: %d\n" (solve2 input_lines)

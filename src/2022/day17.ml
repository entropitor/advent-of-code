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
  ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>" |> String.split_on_char '\n'
  |> List.to_seq

let input_lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

type direction = Left | Right

(* ^  x -> *)
(* | *)
(* y *)

(* %### *)

(* .#. *)
(* ### *)
(* %#. % is not block itself! *)

(* ..# *)
(* ..# *)
(* %## *)

(* # *)
(* # *)
(* # *)
(* % *)

(* ## *)
(* %# *)
type block = Horizontal | Plus | ReverseL | Vertical | Block

let blocks = [ Horizontal; Plus; ReverseL; Vertical; Block ]

let parse lines =
  lines |> SeqExt.hd |> String.to_seq
  |> Seq.map (function
       | '>' -> Right
       | '<' -> Left
       | _ -> failwith "unexpected input token")

let pixels block x y =
  match block with
  | Horizontal -> [ (x, y); (x + 1, y); (x + 2, y); (x + 3, y) ]
  | Plus ->
      [ (x + 1, y); (x, y + 1); (x + 1, y + 1); (x + 2, y + 1); (x + 1, y + 2) ]
  | ReverseL ->
      [ (x, y); (x + 1, y); (x + 2, y); (x + 2, y + 1); (x + 2, y + 2) ]
  | Vertical -> [ (x, y); (x, y + 1); (x, y + 2); (x, y + 3) ]
  | Block -> [ (x, y); (x + 1, y); (x, y + 1); (x + 1, y + 1) ]

let generate_seq lines =
  let jet_stream = parse lines |> Array.of_seq in
  let jet_index = ref 0 in
  let get_next_jet () =
    let result = jet_stream.(!jet_index) in
    jet_index := (!jet_index + 1) mod Array.length jet_stream;
    result
  in

  let highest_block = ref (-1) in

  let map_height = 5000 in
  let map = Array.init map_height (fun _ -> Array.make 7 false) in
  let offset = ref 0 in
  let inc_offset () =
    offset := !offset + (map_height / 2);
    let half_height = map_height / 2 in
    for i = 0 to half_height - 1 do
      map.(i) <- map.(i + half_height);
      map.(i + half_height) <- Array.make 7 false
    done
  in
  let pixel_in_map_taken = function
    | px, _ when px < 0 -> true
    | px, _ when px >= 7 -> true
    | _, py when py < 0 -> true
    | px, py -> map.(py - !offset).(px)
  in

  let _dbg_print_map () =
    let str =
      Seq.init (!highest_block + 4) (fun i ->
          map.(i) |> Array.to_seq
          |> Seq.map (fun x -> if x then '#' else '.')
          |> String.of_seq)
      |> List.of_seq |> List.rev |> String.concat "\n"
    in
    Printf.printf "Map:\n\n%s\n\n" str
  in

  let blocks_stream = blocks |> List.to_seq |> Seq.cycle in
  blocks_stream
  |> Seq.mapi (fun blocki block ->
         (* Printf.printf "Block %d\n" blocki; *)
         if blocki mod 1_000_000 = 0 then (
           Printf.printf "Block %d\n" blocki;
           flush_all ());
         if !highest_block - !offset > map_height - 300 then (
           inc_offset ();
           (* Printf.printf "Increasing offset at block=%d height=%d\n" blocki *)
           (*   !highest_block; *)
           ());
         (* Each rock appears so that its left edge is two units away from the left wall and its bottom edge is three units above the highest rock in the room (or the floor, if there isn't one). *)
         let x = ref 2 in
         let y = ref (!highest_block + 4) in
         let fallen = ref false in
         let push () =
           let new_x =
             match get_next_jet () with Left -> !x - 1 | Right -> !x + 1
           in
           let new_pixels = pixels block new_x !y in
           if not (List.exists pixel_in_map_taken new_pixels) then x := new_x
         in
         let downward () =
           let new_y = !y - 1 in
           let new_pixels = pixels block !x new_y in
           if List.exists pixel_in_map_taken new_pixels then (
             fallen := true;
             let final_pixels = pixels block !x !y in
             final_pixels
             |> List.iter (fun (px, py) -> map.(py - !offset).(px) <- true);
             highest_block :=
               final_pixels |> List.map snd |> List.fold_left max !highest_block;
             (* Printf.printf "Stopped at y=%d, highest_block=%d\n" !y *)
             (*   !highest_block; *)
             ())
           else y := new_y
         in
         while not !fallen do
           push ();
           downward ()
         done;
         (!highest_block + 1, (block, !jet_index)))

let solve1 lines = lines |> generate_seq |> Seq.drop 2021 |> SeqExt.hd |> fst;;

Printf.printf "Part 1 (example): %d\n" (solve1 lines);;
Printf.printf "Part 1: %d\n" (solve1 input_lines);;
flush stdout

let solve2 lines =
  let max_length = (parse lines |> Seq.length) * List.length blocks * 4 in
  let full_seq = lines |> generate_seq |> Seq.memoize in
  let full_seq_pos = full_seq |> Seq.map snd in
  (* https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_tortoise_and_hare *)
  let nu =
    let hoare_seq =
      full_seq_pos |> SeqExt.indexed
      |> Seq.filter (fun (i, _) -> i mod 2 = 0)
      |> Seq.map snd
    in
    Seq.zip full_seq_pos hoare_seq
    |> SeqExt.indexed |> Seq.drop 1
    |> Seq.drop_while (fun (_, (tortoise, hoare)) -> tortoise <> hoare)
    |> SeqExt.hd |> fst
  in
  let mu =
    let hoare_seq = full_seq_pos |> Seq.drop (2 * nu) in
    Seq.zip full_seq_pos hoare_seq
    |> SeqExt.indexed
    |> Seq.drop_while (fun (_, (tortoise, hare)) -> tortoise <> hare)
    |> SeqExt.hd |> fst
  in
  let lambda =
    let tortoise = full_seq_pos |> Seq.drop mu |> SeqExt.hd in
    full_seq_pos |> Seq.drop mu |> SeqExt.indexed |> Seq.drop 1
    |> Seq.drop_while (fun (_, hare) -> tortoise <> hare)
    |> SeqExt.hd |> fst
  in

  Printf.printf "Lambda=%d; mu=%d; nu=%d\n" lambda mu nu;

  let height_at x = full_seq |> Seq.drop x |> SeqExt.hd |> fst in
  let height_mu = height_at mu in
  let height_period =
    let height_mu_plus_lambda = height_at (mu + lambda) in
    height_mu_plus_lambda - height_mu
  in

  Printf.printf "mu=%d; mu+lambda=%d; mu+lambda*2=%d; mu+lambda*3=%d\n"
    (height_at mu)
    (height_at (mu + lambda))
    (height_at (mu + (2 * lambda)))
    (height_at (mu + (3 * lambda)));

  let height_at_smart x =
    if x < mu then height_at x
    else
      let rest = x - mu in
      (height_period * (rest / lambda)) + height_at (mu + (rest mod lambda))
  in

  Printf.printf "mu=%d; mu+lambda=%d; mu+lambda*2=%d; mu+lambda*3=%d\n"
    (height_at_smart mu)
    (height_at_smart (mu + lambda))
    (height_at_smart (mu + (2 * lambda)))
    (height_at_smart (mu + (3 * lambda)));

  height_at_smart 1_000_000_000_000 - 1
;;

Printf.printf "Part 2 (example): %d\nPart 2 (exp-sol): 1514285714288\n"
  (solve2 lines)
;;

Printf.printf "Part 2: %d\n" (solve2 input_lines)

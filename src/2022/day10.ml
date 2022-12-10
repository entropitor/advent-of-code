let lines = "noop\naddx 3\naddx -5" |> String.split_on_char '\n' |> List.to_seq

let lines2 =
  "addx 15\n\
   addx -11\n\
   addx 6\n\
   addx -3\n\
   addx 5\n\
   addx -1\n\
   addx -8\n\
   addx 13\n\
   addx 4\n\
   noop\n\
   addx -1\n\
   addx 5\n\
   addx -1\n\
   addx 5\n\
   addx -1\n\
   addx 5\n\
   addx -1\n\
   addx 5\n\
   addx -1\n\
   addx -35\n\
   addx 1\n\
   addx 24\n\
   addx -19\n\
   addx 1\n\
   addx 16\n\
   addx -11\n\
   noop\n\
   noop\n\
   addx 21\n\
   addx -15\n\
   noop\n\
   noop\n\
   addx -3\n\
   addx 9\n\
   addx 1\n\
   addx -3\n\
   addx 8\n\
   addx 1\n\
   addx 5\n\
   noop\n\
   noop\n\
   noop\n\
   noop\n\
   noop\n\
   addx -36\n\
   noop\n\
   addx 1\n\
   addx 7\n\
   noop\n\
   noop\n\
   noop\n\
   addx 2\n\
   addx 6\n\
   noop\n\
   noop\n\
   noop\n\
   noop\n\
   noop\n\
   addx 1\n\
   noop\n\
   noop\n\
   addx 7\n\
   addx 1\n\
   noop\n\
   addx -13\n\
   addx 13\n\
   addx 7\n\
   noop\n\
   addx 1\n\
   addx -33\n\
   noop\n\
   noop\n\
   noop\n\
   addx 2\n\
   noop\n\
   noop\n\
   noop\n\
   addx 8\n\
   noop\n\
   addx -1\n\
   addx 2\n\
   addx 1\n\
   noop\n\
   addx 17\n\
   addx -9\n\
   addx 1\n\
   addx 1\n\
   addx -3\n\
   addx 11\n\
   noop\n\
   noop\n\
   addx 1\n\
   noop\n\
   addx 1\n\
   noop\n\
   noop\n\
   addx -13\n\
   addx -19\n\
   addx 1\n\
   addx 3\n\
   addx 26\n\
   addx -30\n\
   addx 12\n\
   addx -1\n\
   addx 3\n\
   addx 1\n\
   noop\n\
   noop\n\
   noop\n\
   addx -9\n\
   addx 18\n\
   addx 1\n\
   addx 2\n\
   noop\n\
   noop\n\
   addx 9\n\
   noop\n\
   noop\n\
   noop\n\
   addx -1\n\
   addx 2\n\
   addx -37\n\
   addx 1\n\
   addx 3\n\
   noop\n\
   addx 15\n\
   addx -21\n\
   addx 22\n\
   addx -6\n\
   addx 1\n\
   noop\n\
   addx 2\n\
   addx 1\n\
   noop\n\
   addx -10\n\
   noop\n\
   noop\n\
   addx 20\n\
   addx 1\n\
   addx 2\n\
   addx 2\n\
   addx -6\n\
   addx -11\n\
   noop\n\
   noop\n\
   noop" |> String.split_on_char '\n' |> List.to_seq

let input_lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

type command = Noop | Add of int

let parse lines =
  lines
  |> Seq.map (fun line ->
         match line with
         | "noop" -> Noop
         | _ -> Scanf.sscanf line "addx %d" (fun i -> Add i))

let run commands =
  let state = ref 1 in
  commands
  |> Seq.flat_map (fun command ->
         match command with
         | Noop -> List.to_seq [ !state ]
         | Add x ->
             let old_state = !state in
             state := old_state + x;
             List.to_seq [ old_state; !state ])
  |> Seq.cons 1 |> Seq.memoize

let solve1 lines =
  let commands = parse lines in
  run commands
  |> Seq.mapi (fun i value -> if (i - 20) mod 40 = 0 then value * i else 0)
  |> Seq.fold_left ( + ) 0
;;

(* solve lines2 |> List.of_seq;; *)
solve1 lines2;;
Printf.printf "Part 1: %d\n" (solve1 input_lines)

(* --------- *)

let solve2 lines =
  let commands = parse lines in
  run commands
  |> Seq.mapi (fun i sprite ->
         let column = i mod 40 in
         if abs (column - sprite) <= 1 then '#' else '.')
  |> Seq.mapi (fun i c -> (i, c))
  |> Seq.group (fun a b -> fst b mod 40 != 0)
  |> Seq.map (fun seq -> seq |> Seq.map snd |> String.of_seq)
  |> Seq.take 6 |> List.of_seq |> String.concat "\n"

let ( >> ) f g x = x |> f |> g;;

(* lines2 |> parse |> run *)
(* |> Seq.mapi (fun i c -> (i, c)) *)
(* |> List.of_seq *)
(* |> Seq.group (fun a b -> fst b mod 40 != 0) *)
(* |> Seq.map (Seq.map snd >> List.of_seq) *)
(* |> List.of_seq *)
Printf.printf "\n\n\n%s\n\n\n" (solve2 lines2);;
Printf.printf "Part 2: \n\n\n%s\n\n\n" (solve2 input_lines)
(* solve2 lines2;; *)
(* Printf.printf "Part 1: %d\n" (solve1 input_lines) *)

(* ###..#....#..#.#....#..#.###..####.#..#. *)
(* ..####...#######..##....####.........##. *)
(* ...........#....###.........#........... *)
(* ###..#....#..#.#....#.#..#..#..#...#..#. *)
(* ...###..###.......#####....####..######. *)
(* ........................................ *)

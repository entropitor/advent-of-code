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
  "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each \
   obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 \
   obsidian.\n\
   Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each \
   obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 \
   obsidian." |> String.split_on_char '\n' |> List.to_seq

let input_lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

type blueprint = {
  id : int;
  ore_for_ore_robot : int;
  ore_for_clay_robot : int;
  ore_and_clay_for_obsidian_robot : int * int;
  ore_and_obsidian_for_geode_robot : int * int;
}

type state = {
  nb_turns_left : int;
  nb_ore : int;
  nb_ore_robot : int;
  nb_clay : int;
  nb_clay_robot : int;
  nb_obsidian : int;
  nb_obsidian_robot : int;
  nb_geode : int;
  nb_geode_robot : int;
}

let parse lines =
  lines
  |> Seq.map (fun line ->
         Scanf.sscanf line
           "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs \
            %d ore. Each obsidian robot costs %d ore and %d clay. Each geode \
            robot costs %d ore and %d obsidian."
           (fun
             blueprint_id
             ore_robot_ore
             clay_robot_ore
             obsidian_robot_ore
             obsidian_robot_clay
             geode_robot_ore
             geode_robot_obsidian
           ->
             {
               id = blueprint_id;
               ore_for_ore_robot = ore_robot_ore;
               ore_for_clay_robot = clay_robot_ore;
               ore_and_clay_for_obsidian_robot =
                 (obsidian_robot_ore, obsidian_robot_clay);
               ore_and_obsidian_for_geode_robot =
                 (geode_robot_ore, geode_robot_obsidian);
             }))

let solve_blueprint nb_turns (blueprint : blueprint) =
  Printf.printf "Solving blueprint %d\n" blueprint.id;
  flush stdout;
  let memo_tbl = Hashtbl.create 10000 in
  let max_ore =
    [
      blueprint.ore_for_ore_robot;
      blueprint.ore_for_clay_robot;
      fst blueprint.ore_and_clay_for_obsidian_robot;
      fst blueprint.ore_and_obsidian_for_geode_robot;
    ]
    |> List.fold_left max 0
  in
  let best_so_far = ref 0 in
  let rec find_max_nb_geode from =
    let find () =
      (* Printf.printf *)
      (* "Calculating turns=%d; ore=%d; clay=%d; obsidian=%d geode=%d\n\ *)
         (*    Robot: ore=%d; clay=%d; obsidian=%d geode=%d\n\n" *)
      (*   from.nb_turns_left from.nb_ore from.nb_clay from.nb_obsidian *)
      (*   from.nb_geode from.nb_ore_robot from.nb_clay_robot *)
      (*   from.nb_obsidian_robot from.nb_geode_robot; *)
      if
        from.nb_geode
        + (from.nb_geode_robot * from.nb_turns_left)
        + (from.nb_turns_left * (from.nb_turns_left - 1) / 2)
        <= !best_so_far
      then 0
      else if from.nb_turns_left = 0 then (
        let result = from.nb_geode in
        best_so_far := max !best_so_far result;
        result)
      else
        let next_turn =
          {
            from with
            nb_turns_left = from.nb_turns_left - 1;
            nb_ore = from.nb_ore + from.nb_ore_robot;
            nb_clay = from.nb_clay + from.nb_clay_robot;
            nb_obsidian = from.nb_obsidian + from.nb_obsidian_robot;
            nb_geode = from.nb_geode + from.nb_geode_robot;
          }
        in
        let max_next_turn = ref (find_max_nb_geode next_turn) in

        if
          from.nb_ore >= fst blueprint.ore_and_obsidian_for_geode_robot
          && from.nb_obsidian >= snd blueprint.ore_and_obsidian_for_geode_robot
        then
          max_next_turn :=
            max !max_next_turn
              (find_max_nb_geode
                 {
                   next_turn with
                   nb_geode_robot = next_turn.nb_geode_robot + 1;
                   nb_ore =
                     next_turn.nb_ore
                     - fst blueprint.ore_and_obsidian_for_geode_robot;
                   nb_obsidian =
                     next_turn.nb_obsidian
                     - snd blueprint.ore_and_obsidian_for_geode_robot;
                 });

        if
          from.nb_ore >= fst blueprint.ore_and_clay_for_obsidian_robot
          && from.nb_clay >= snd blueprint.ore_and_clay_for_obsidian_robot
          && not
               (from.nb_obsidian_robot
               >= snd blueprint.ore_and_obsidian_for_geode_robot)
        then
          max_next_turn :=
            max !max_next_turn
              (find_max_nb_geode
                 {
                   next_turn with
                   nb_obsidian_robot = next_turn.nb_obsidian_robot + 1;
                   nb_ore =
                     next_turn.nb_ore
                     - fst blueprint.ore_and_clay_for_obsidian_robot;
                   nb_clay =
                     next_turn.nb_clay
                     - snd blueprint.ore_and_clay_for_obsidian_robot;
                 });

        if
          from.nb_ore >= blueprint.ore_for_clay_robot
          && not
               (from.nb_clay_robot
               >= snd blueprint.ore_and_clay_for_obsidian_robot)
        then
          max_next_turn :=
            max !max_next_turn
              (find_max_nb_geode
                 {
                   next_turn with
                   nb_clay_robot = next_turn.nb_clay_robot + 1;
                   nb_ore = next_turn.nb_ore - blueprint.ore_for_clay_robot;
                 });

        if
          from.nb_ore >= blueprint.ore_for_ore_robot
          && not (from.nb_ore_robot >= max_ore)
        then
          max_next_turn :=
            max !max_next_turn
              (find_max_nb_geode
                 {
                   next_turn with
                   nb_ore_robot = next_turn.nb_ore_robot + 1;
                   nb_ore = next_turn.nb_ore - blueprint.ore_for_ore_robot;
                 });

        !max_next_turn
    in
    match Hashtbl.find_opt memo_tbl from with
    | Some x -> x
    | None ->
        let value = find () in
        Hashtbl.replace memo_tbl from value;
        value
  in
  let result =
    find_max_nb_geode
      {
        nb_turns_left = nb_turns;
        nb_ore = 0;
        nb_ore_robot = 1;
        nb_clay = 0;
        nb_clay_robot = 0;
        nb_obsidian = 0;
        nb_obsidian_robot = 0;
        nb_geode = 0;
        nb_geode_robot = 0;
      }
  in
  Printf.printf "Solved blueprint %d=%d\n" blueprint.id result;
  result

let solve1 lines =
  parse lines
  |> Seq.mapi (fun i b -> (i + 1) * solve_blueprint 24 b)
  |> Seq.fold_left ( + ) 0

let solve2 lines =
  parse lines |> Seq.take 3
  |> Seq.map (solve_blueprint 32)
  |> Seq.fold_left (fun a b -> a * b) 1
;;

(* Printf.printf "Part 1 (example): %d\n" (solve1 lines);; *)
(* flush stdout;; *)
(* Printf.printf "Part 1: %d\n" (solve1 input_lines);; *)
(* flush stdout;; *)
Printf.printf "Part 2 (example): %d\n" (solve2 lines);;
flush stdout;;
Printf.printf "Part 2: %d\n" (solve2 input_lines);;
flush stdout

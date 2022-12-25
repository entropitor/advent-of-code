let ( >> ) f g x = f x |> g
let const x y = x
let id x = x

let lines =
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\n\
   Valve BB has flow rate=13; tunnels lead to valves CC, AA\n\
   Valve CC has flow rate=2; tunnels lead to valves DD, BB\n\
   Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE\n\
   Valve EE has flow rate=3; tunnels lead to valves FF, DD\n\
   Valve FF has flow rate=0; tunnels lead to valves EE, GG\n\
   Valve GG has flow rate=0; tunnels lead to valves FF, HH\n\
   Valve HH has flow rate=22; tunnel leads to valve GG\n\
   Valve II has flow rate=0; tunnels lead to valves AA, JJ\n\
   Valve JJ has flow rate=21; tunnel leads to valve II"
  |> String.split_on_char '\n' |> List.to_seq

let input_lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

let parse_valves others =
  others |> String.split_on_char ','
  |> List.mapi (fun i s -> Scanf.sscanf s (if i = 0 then "%s" else " %s") id)

module StringSet = Set.Make (struct
  type t = string

  let compare = compare
end)

let parse lines =
  lines
  |> Seq.map (fun line ->
         Scanf.sscanf line
           "Valve %s has flow rate=%d; tunnel%s lead%s to valve%s %s@;"
           (fun name flow _ _ _ others -> (name, (flow, parse_valves others))))
  |> Hashtbl.of_seq

let solve1 lines =
  let tbl = parse lines in
  let queue = Queue.create () in
  Queue.push (30, "AA", StringSet.empty, 0) queue;
  let best = ref 0 in
  let visited_tbl = Hashtbl.create 100 in
  let visit_node nb_turns_left other opened score =
    let visited_node_without_score = (other, opened, nb_turns_left) in
    let best_score =
      Hashtbl.find_opt visited_tbl visited_node_without_score
      |> Option.fold ~none:(-1) ~some:id
    in
    if score > best_score then (
      Queue.push (nb_turns_left, other, opened, score) queue;
      Hashtbl.replace visited_tbl visited_node_without_score score)
  in
  while Queue.length queue > 0 do
    let nb_turns_left, at, opened, score = Queue.pop queue in
    (* Printf.printf "Visiting %s (left=%d; score=%d)\n" at nb_turns_left score; *)
    if nb_turns_left > 0 then (
      let flow, others = Hashtbl.find tbl at in
      others
      |> List.iter (fun other ->
             visit_node (nb_turns_left - 1) other opened score);
      if (not (StringSet.mem at opened)) && flow > 0 then
        visit_node (nb_turns_left - 1) at
          (opened |> StringSet.add at)
          (score + (flow * (nb_turns_left - 1))));
    if score > !best then best := score
  done;
  !best
;;

(* parse lines;; *)
(* solve1 lines;; *)
Printf.printf "Part 1 (example): %d\n" (solve1 lines);;
flush stdout

(* Printf.printf "Part 1: %d\n" (solve1 input_lines) *)

(* ============= *)

let merge_lists a b =
  let rec with_acc a b acc =
    match (a, b) with
    | [], _ -> acc
    | _, [] -> acc
    | c :: d, e ->
        let new_acc = List.rev_append (List.map (fun x -> (c, x)) e) acc in
        with_acc d e new_acc
  in
  with_acc a b []

module VisitedSet = Set.Make (struct
  type t = string * string * StringSet.t * int * int

  let compare = compare
end)

let solve2 lines =
  let tbl = parse lines in
  let stack = Stack.create () in
  Stack.push (26, "AA", "AA", StringSet.empty, 0, 0) stack;
  let best = ref 0 in
  let best_flow_tbl = Hashtbl.create 10000 in
  (* let nb_possible_valves = *)
  (*   tbl |> Hashtbl.to_seq *)
  (*   |> Seq.filter (fun (name, (flow, _)) -> flow > 0) *)
  (*   |> Seq.length *)
  (* in *)
  let visited = ref VisitedSet.empty in
  let visit_node nb_turns_left you elephant opened flow score =
    let best_flow_node = (you, elephant, nb_turns_left) in
    let best_flow_node_alt = (elephant, you, nb_turns_left) in
    let best_flow =
      Hashtbl.find_opt best_flow_tbl best_flow_node
      |> Option.fold ~none:(-1) ~some:id
    in
    let best_flow_alt =
      Hashtbl.find_opt best_flow_tbl best_flow_node_alt
      |> Option.fold ~none:(-1) ~some:id
    in
    let visited_node = (you, elephant, opened, nb_turns_left, score) in
    if
      flow >= max best_flow best_flow_alt
      && not (VisitedSet.mem visited_node !visited)
    then (
      visited := VisitedSet.add visited_node !visited;
      Stack.push (nb_turns_left, you, elephant, opened, flow, score) stack;
      Hashtbl.replace best_flow_tbl best_flow_node_alt flow;
      Hashtbl.replace best_flow_tbl best_flow_node flow)
  in
  while Stack.length stack > 0 do
    let nb_turns_left, you_at, elephant_at, opened, flow, score =
      Stack.pop stack
    in
    (* Printf.printf "Visiting %s,%s (left=%d; score=%d)\n" you_at elephant_at *)
    (*   nb_turns_left score; *)
    (if
     nb_turns_left > 0
     (* && StringSet.cardinal opened < nb_possible_valves *)
     (* && flow *)
     (*    >= (Hashtbl.find_opt best_flow_tbl (you_at, elephant_at, nb_turns_left) *)
     (*       |> Option.fold ~none:(-1) ~some:id) *)
    then
     let you_flow, you_next = Hashtbl.find tbl you_at in
     let elephant_flow, elephant_next = Hashtbl.find tbl elephant_at in
     let you_options =
       let mapped = List.map Option.some you_next in
       if StringSet.mem you_at opened || you_flow = 0 then mapped
       else None :: mapped
     in
     let elephant_options =
       let mapped = List.map Option.some elephant_next in
       if StringSet.mem elephant_at opened || elephant_flow = 0 then mapped
       else None :: mapped
     in
     let options = merge_lists you_options elephant_options in
     let next_turn = nb_turns_left - 1 in
     options
     |> List.filter (fun options ->
            if you_at <> elephant_at then true
            else
              match options with
              | None, None -> false
              | Some _, None -> true
              | None, Some _ -> true
              | Some x, Some y -> true)
     |> List.iter (function
          | None, None ->
              visit_node next_turn you_at elephant_at
                (opened |> StringSet.add you_at |> StringSet.add elephant_at)
                (flow + you_flow + elephant_flow)
                (score + ((you_flow + elephant_flow) * next_turn))
          | None, Some y ->
              visit_node next_turn you_at y
                (opened |> StringSet.add you_at)
                (flow + you_flow)
                (score + (you_flow * next_turn))
          | Some x, None ->
              visit_node next_turn x elephant_at
                (opened |> StringSet.add elephant_at)
                (flow + elephant_flow)
                (score + (elephant_flow * next_turn))
          | Some x, Some y -> visit_node next_turn x y opened flow score));
    if score > !best then best := score
  done;
  !best

let solve2_different lines =
  let tbl = parse lines in
  let queue = Queue.create () in
  Queue.push (27, "AA", "AA", StringSet.empty, 0, 0) queue;
  let best = ref 0 in
  let nb_possible_valves =
    tbl |> Hashtbl.to_seq
    |> Seq.filter (fun (name, (flow, _)) -> flow > 0)
    |> Seq.length
  in
  let best_flow_tbl = Hashtbl.create 10000 in
  let get_best_flow node =
    Hashtbl.find_opt best_flow_tbl node |> Option.fold ~none:(-1) ~some:id
  in
  let visit_node nb_turns_left you elephant opened flow score =
    let node = (you, elephant, nb_turns_left) in
    if flow >= get_best_flow node then (
      Queue.push (nb_turns_left, you, elephant, opened, flow, score) queue;
      Hashtbl.replace best_flow_tbl node flow)
  in
  (* let visit_node nb_turns_left you elephant opened flow score = *)
  (*   Queue.push (nb_turns_left, you, elephant, opened, flow, score) queue *)
  (* in *)
  while Queue.length queue > 0 do
    let nb_turns_left, you_at, elephant_at, opened, flow, score =
      Queue.pop queue
    in
    if
      nb_turns_left > 0
      && flow >= get_best_flow (you_at, elephant_at, nb_turns_left)
    then (
      Hashtbl.replace best_flow_tbl (you_at, elephant_at, nb_turns_left) flow;
      Hashtbl.replace best_flow_tbl (elephant_at, you_at, nb_turns_left) flow;
      Printf.printf "Visiting %s,%s (left=%d; score=%d; queue_length=%d)\n"
        you_at elephant_at nb_turns_left score (Queue.length queue);
      let you_flow, you_next = Hashtbl.find tbl you_at in
      let elephant_flow, elephant_next = Hashtbl.find tbl elephant_at in
      let you_options =
        let mapped = List.map Option.some you_next in
        if StringSet.mem you_at opened || you_flow = 0 then mapped
        else None :: mapped
      in
      let elephant_options =
        let mapped = List.map Option.some elephant_next in
        if StringSet.mem elephant_at opened || elephant_flow = 0 then mapped
        else None :: mapped
      in
      let options = merge_lists you_options elephant_options in
      let next_turn = nb_turns_left - 1 in
      if StringSet.cardinal opened = nb_possible_valves then
        visit_node 0 you_at elephant_at opened flow
          (score + (flow * nb_turns_left))
      else
        options
        |> List.filter (fun options ->
               if you_at <> elephant_at then true
               else
                 match options with
                 | None, None -> false
                 | Some _, None -> true
                 | None, Some _ -> true
                 | Some x, Some y -> true)
        |> List.iter (function
             | None, None ->
                 visit_node next_turn you_at elephant_at
                   (opened |> StringSet.add you_at |> StringSet.add elephant_at)
                   (flow + you_flow + elephant_flow)
                   (score + flow)
             | None, Some y ->
                 visit_node next_turn you_at y
                   (opened |> StringSet.add you_at)
                   (flow + you_flow) (score + flow)
             | Some x, None ->
                 visit_node next_turn x elephant_at
                   (opened |> StringSet.add elephant_at)
                   (flow + elephant_flow) (score + flow)
             | Some x, Some y ->
                 visit_node next_turn x y opened flow (score + flow)));
    if score > !best then best := score
  done;
  !best

(*
State:
  - nb_turns_left (iterative, only 2 needed)
  - you_at, elephant_at (59*59=3481)
  - valves_open (2**15=32768)
  =>
  - score

*)

type action = Open of int | MoveTo of int

let solve2_dp lines =
  let tbl = parse lines in
  let tbl_as_seq = tbl |> Hashtbl.to_seq |> Seq.memoize in
  let positions = tbl_as_seq |> Seq.map fst in
  let positions_order_tbl =
    tbl_as_seq
    |> Seq.mapi (fun i (name, (_flow, _next)) -> (name, i))
    |> Hashtbl.of_seq
  in
  let next_tbl =
    tbl_as_seq
    |> Seq.map (fun (name, (_, next)) ->
           ( Hashtbl.find positions_order_tbl name,
             next
             |> List.map (fun next_name ->
                    Hashtbl.find positions_order_tbl next_name) ))
    |> Hashtbl.of_seq
  in
  let flow_tbl =
    tbl_as_seq
    |> Seq.map (fun (name, (flow, _)) ->
           (Hashtbl.find positions_order_tbl name, flow))
    |> Hashtbl.of_seq
  in
  let opened_indices =
    let i = ref (-1) in
    tbl_as_seq
    |> Seq.map (fun (name, (flow, _neighbours)) ->
           if flow = 0 then None
           else (
             i := !i + 1;
             Some !i))
    |> Array.of_seq
  in
  (* let openable_valves = *)
  (*   tbl_as_seq *)
  (*   |> Seq.filter (fun (name, (flow, _)) -> flow > 0) *)
  (*   |> Seq.map (fun (name, (flow, _)) -> (name, flow)) *)
  (*   |> Hashtbl.of_seq *)
  (* in *)
  let nb_openable_valves =
    opened_indices |> Array.to_seq |> Seq.filter Option.is_some |> Seq.length
  in
  let nb_openable_valve_options = 1 lsl nb_openable_valves in
  Printf.printf "Calculating flow\n";
  flush_all ();
  let flow_per_flow_index =
    (* let openable_valves_order = openable_valves |> Hashtbl.to_seq in *)
    (* let openable_valves_order = *)
    (*   openable_valves_order |> Seq.map snd |> Array.of_seq *)
    (* in *)
    Seq.init nb_openable_valve_options (fun flow_index ->
        opened_indices |> Array.to_seq
        |> Seq.mapi (fun at_index opened_index ->
               match opened_index with
               | None -> 0
               | Some opened_index ->
                   if (flow_index asr opened_index) mod 2 = 1 then
                     Hashtbl.find flow_tbl at_index
                   else 0)
        |> Seq.fold_left ( + ) 0)
    |> List.of_seq
    (* let rest = ref i in *)
    (* let total = ref 0 in *)
    (* for bit = 0 to nb_openable_valves do *)
    (*   if !rest mod 2 = 1 then total := !total + openable_valves_order.(bit); *)
    (*   rest := !rest asr 1 *)
    (* done; *)
    (* !total) *)
  in
  Printf.printf "Done calculating flow\n";
  flush_all ();
  let nb_possible_positions = Seq.length positions in
  let gen_dp () =
    Array.init nb_possible_positions (fun _ ->
        Array.init nb_possible_positions (fun _ ->
            Array.make nb_openable_valve_options 0))
  in
  let dp = ref (gen_dp ()) in
  (* let pow_of_2 x = 2. ** float_of_int x |> int_of_float in *)
  let options at_index flow_index =
    let opened_index = opened_indices.(at_index) in
    let next = Hashtbl.find next_tbl at_index in
    let mapped = List.map (fun x -> MoveTo x) next in
    match opened_index with
    | None -> mapped
    | Some opened_index ->
        if (flow_index asr opened_index) mod 2 = 1 then mapped
        else Open (1 lsl opened_index) :: mapped
  in
  Printf.printf "Turn %d\n" 0;
  (* turns_left = 1 *)
  positions
  |> Seq.iteri (fun you_at_index _you_at ->
         positions
         |> Seq.iteri (fun elephant_at_index _elephant_at ->
                flow_per_flow_index
                |> List.iteri (fun flow_index flow ->
                       !dp.(you_at_index).(elephant_at_index).(flow_index) <-
                         flow)));
  (* turns_left = 1 + i *)
  for i = 1 to 25 do
    Printf.printf "Turn %d\n" i;
    flush_all ();
    let next_dp = gen_dp () in
    positions
    |> Seq.iteri (fun you_at_index _you_at ->
           positions
           |> Seq.iteri (fun elephant_at_index _elephant_at ->
                  flow_per_flow_index
                  |> List.iteri (fun flow_index flow_for_index ->
                         let options =
                           let you_options = options you_at_index flow_index in
                           let elephant_options =
                             options elephant_at_index flow_index
                           in
                           merge_lists you_options elephant_options
                         in
                         let best_next =
                           options
                           |> List.filter (fun options ->
                                  match options with
                                  | Open x, Open y -> x <> y
                                  | _, _ -> true)
                           |> List.map (function
                                | Open x, Open y ->
                                    !dp.(you_at_index).(elephant_at_index).(flow_index
                                                                            + x
                                                                            + y)
                                | Open x, MoveTo y ->
                                    !dp.(you_at_index).(y).(flow_index + x)
                                | MoveTo x, Open y ->
                                    !dp.(x).(elephant_at_index).(flow_index + y)
                                | MoveTo x, MoveTo y -> !dp.(x).(y).(flow_index))
                           |> List.fold_left max 0
                         in
                         next_dp.(you_at_index).(elephant_at_index).(flow_index) <-
                           best_next + flow_for_index)));
    dp := next_dp
  done;
  let aa = Hashtbl.find positions_order_tbl "AA" in
  !dp.(aa).(aa).(0)
(* Array.fold_left max 0 !dp.(aa).(aa) *)
(* Array.fold_left *)
(*   (fun acc arr2d -> *)
(*     max acc *)
(*       (Array.fold_left *)
(*          (fun acc arr -> max acc (Array.fold_left max 0 arr)) *)
(*          0 arr2d)) *)
(*   0 !dp *)
;;

Printf.printf "Part 2 (example): %d\n" (solve2_dp lines);;
flush stdout;;
Printf.printf "Part 2: %d\n" (solve2_dp input_lines)

(* Printf.printf "Part 2 (example): %d\n" (solve2 lines);; *)
(* flush stdout;; *)
(* Printf.printf "Part 2 (example) differently: %d\n" (solve2_different lines);; *)
(* flush stdout *)
(* Printf.printf "Part 2: %d\n" (solve2 input_lines) *)

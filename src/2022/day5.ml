let example_lines =
  "    [D]    \n\
   [N] [C]    \n\
   [Z] [M] [P]\n\
  \ 1   2   3 \n\n\
   move 1 from 2 to 1\n\
   move 3 from 1 to 3\n\
   move 2 from 2 to 1\n\
   move 1 from 1 to 2" |> String.split_on_char '\n' |> List.to_seq

let lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

let parse lines =
  let crates =
    lines
    |> Seq.take_while (fun line -> String.length line != 0)
    |> Seq.map (fun line ->
           let nb_stacks = (String.length line + 1) / 4 in
           List.init nb_stacks (fun x -> (4 * x) + 1)
           |> List.map (fun i ->
                  if line.[i] == ' ' then None else Some line.[i]))
    |> List.of_seq |> List.rev |> List.to_seq |> Seq.drop 1 |> List.of_seq
  in
  let nb_stacks = List.length (List.hd crates) in
  let crates_by_id = Hashtbl.create nb_stacks in
  let _ =
    List.init nb_stacks (fun x -> x)
    |> List.map (fun i -> Hashtbl.add crates_by_id i (Stack.create ()))
  in
  let _ =
    crates
    |> List.map (fun crates_row ->
           crates_row
           |> List.mapi (fun i crate ->
                  match crate with
                  | None -> ()
                  | Some c -> Hashtbl.find crates_by_id i |> Stack.push c))
  in
  let instructions =
    lines
    |> Seq.drop_while (fun line -> String.length line != 0)
    |> Seq.drop 1
    |> Seq.map (fun line ->
           Scanf.sscanf line "move %d from %d to %d" (fun a b c ->
               (a, b - 1, c - 1)))
  in
  (crates_by_id, instructions)

let solve keep_order lines =
  let tbl, instructions = parse lines in
  let _ =
    instructions
    |> Seq.map (fun (nb, from, til) ->
           if keep_order then
             let _ =
               List.init nb (fun _ ->
                   let popped = Hashtbl.find tbl from |> Stack.pop in
                   popped)
               |> List.rev
               |> List.map (fun popped ->
                      Hashtbl.find tbl til |> Stack.push popped)
             in
             ()
           else
             let _ =
               List.init nb (fun _ ->
                   let popped = Hashtbl.find tbl from |> Stack.pop in
                   Hashtbl.find tbl til |> Stack.push popped)
             in
             ())
    |> List.of_seq
  in
  List.init (Hashtbl.length tbl) (fun i -> Hashtbl.find tbl i |> Stack.top)
  |> List.to_seq |> String.of_seq
;;

Printf.printf "Part 1 (example): %s\n" (solve false example_lines);;
Printf.printf "Part 1: %s\n" (solve false lines);;
Printf.printf "Part 2 (example): %s\n" (solve true example_lines);;
Printf.printf "Part 2: %s\n" (solve true lines)

let lines =
  "Monkey 0:\n\
  \  Starting items: 79, 98\n\
  \  Operation: new = old * 19\n\
  \  Test: divisible by 23\n\
  \    If true: throw to monkey 2\n\
  \    If false: throw to monkey 3\n\n\
   Monkey 1:\n\
  \  Starting items: 54, 65, 75, 74\n\
  \  Operation: new = old + 6\n\
  \  Test: divisible by 19\n\
  \    If true: throw to monkey 2\n\
  \    If false: throw to monkey 0\n\n\
   Monkey 2:\n\
  \  Starting items: 79, 60, 97\n\
  \  Operation: new = old * old\n\
  \  Test: divisible by 13\n\
  \    If true: throw to monkey 1\n\
  \    If false: throw to monkey 3\n\n\
   Monkey 3:\n\
  \  Starting items: 74\n\
  \  Operation: new = old + 3\n\
  \  Test: divisible by 17\n\
  \    If true: throw to monkey 0\n\
  \    If false: throw to monkey 1" |> String.split_on_char '\n' |> List.to_seq

let input_lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

type operation = Add of int | Multiply of int | Square

type monkey = {
  items : int Queue.t;
  op : operation;
  test : int;
  trueMonkey : int;
  falseMonkey : int;
  mutable nb_items_seen : int;
}

let skip sub str =
  let sub_length = String.length sub in
  String.sub str sub_length (String.length str - sub_length)

let id x = x

let parse_monkey lines =
  let[@warning "-8"] [
                   monkeyLine;
                   starting_items;
                   operation;
                   test;
                   trueMonkey;
                   falseMonkey;
                 ] =
    lines |> List.of_seq
  in
  let items =
    skip "  Starting items: " starting_items
    |> String.split_on_char ',' |> List.to_seq
    |> Seq.mapi (fun i s -> Scanf.sscanf s (if i == 0 then "%d" else " %d") id)
    |> Queue.of_seq
  in

  let operation =
    let operation = skip "  Operation: new = " operation in
    if operation = "old * old" then Square
    else
      Scanf.sscanf operation "old %c %d" (fun c d ->
          match c with
          | '*' -> Multiply d
          | '+' -> Add d
          | _ -> failwith "unexpected")
  in
  let test = Scanf.sscanf test "  Test: divisible by %d" id in
  let trueMonkey =
    Scanf.sscanf trueMonkey "    If true: throw to monkey %d" id
  in
  let falseMonkey =
    Scanf.sscanf falseMonkey "    If false: throw to monkey %d" id
  in
  { items; op = operation; test; trueMonkey; falseMonkey; nb_items_seen = 0 }

let parse lines =
  lines
  |> Seq.group (fun _ line -> line <> "")
  |> Seq.mapi (fun i group -> if i == 0 then group else group |> Seq.drop 1)
  |> Seq.map parse_monkey |> Array.of_seq

let apply_operation op x =
  match op with Square -> x * x | Add y -> x + y | Multiply y -> x * y

let play_round is_part1 monkeys =
  let divider =
    monkeys |> Array.to_seq
    |> Seq.map (fun monkey -> monkey.test)
    |> Seq.fold_left (fun x y -> x * y) 1
  in
  monkeys
  |> Array.iter (fun monkey ->
         monkey.items
         |> Queue.iter (fun item ->
                monkey.nb_items_seen <- monkey.nb_items_seen + 1;
                let new_item = item |> apply_operation monkey.op in
                let new_item =
                  if is_part1 then new_item / 3 else new_item mod divider
                in
                let new_index =
                  if new_item mod monkey.test = 0 then monkey.trueMonkey
                  else monkey.falseMonkey
                in
                Queue.push new_item monkeys.(new_index).items);
         Queue.clear monkey.items);
  monkeys

let solve is_part1 lines =
  let monkeys = parse lines in
  for i = 1 to if is_part1 then 20 else 10000 do
    play_round is_part1 monkeys |> ignore
  done;
  monkeys |> Array.sort (fun a z -> compare z.nb_items_seen a.nb_items_seen);
  monkeys.(0).nb_items_seen * monkeys.(1).nb_items_seen

let solve1 lines = solve true lines

let debug_monkeys monkeys =
  monkeys
  |> Array.map (fun monkey ->
         (monkey, monkey.items |> Queue.to_seq |> List.of_seq))
;;

parse lines |> debug_monkeys;;
parse lines |> play_round true |> debug_monkeys;;
solve1 lines;;
Printf.printf "Part 1: %d\n" (solve1 input_lines);;
solve false lines;;
Printf.printf "Part 2: %d\n" (solve false input_lines)

let lines =
  "[1,1,3,1,1]\n\
   [1,1,5,1,1]\n\n\
   [[1],[2,3,4]]\n\
   [[1],4]\n\n\
   [9]\n\
   [[8,7,6]]\n\n\
   [[4,4],4,4]\n\
   [[4,4],4,4,4]\n\n\
   [7,7,7,7]\n\
   [7,7,7]\n\n\
   []\n\
   [3]\n\n\
   [[[]]]\n\
   [[]]\n\n\
   [1,[2,[3,[4,[5,6,7]]]],8,9]\n\
   [1,[2,[3,[4,[5,6,0]]]],8,9]" |> String.split_on_char '\n' |> List.to_seq

let input_lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

type input = Number of int | InputList of input list

let id x = x

module OptExt = struct
  let orElse opt x = match opt with None -> x | Some _ -> opt
end

module SeqExt = struct
  let hd seq =
    match seq () with
    | Seq.Nil -> failwith "Nothing in sequence"
    | Seq.Cons (first, _) -> first
end

let ( >> ) f g x = x |> f |> g

let rec parse_line line =
  (* Printf.printf "Parsing '%s'\n" line; *)
  let find_end_list from =
    let open_index = String.index_from line from '[' in
    let close_index =
      line |> String.to_seq |> Seq.drop open_index
      |> Seq.mapi (fun i c -> (i, c))
      |> Seq.scan
           (fun (acc, nb) (i, c) ->
             match c with
             | '[' -> (acc, nb + 1)
             | ']' ->
                 ((if nb = 1 then OptExt.orElse acc (Some i) else None), nb - 1)
             | _ -> (acc, nb))
           (None, 0)
      |> Seq.drop_while (fst >> Option.is_none)
      |> SeqExt.hd |> fst |> Option.get
    in
    (open_index, from + close_index)
  in
  let i = ref 0 in
  let j = ref 0 in
  let items = Queue.create () in
  while !i < String.length line do
    match line.[!i] with
    | '[' ->
        let opens, closes = find_end_list !i in
        (* Printf.printf "Found list (%d, %d)\n" opens closes; *)
        let parsed =
          parse_line (String.sub line (opens + 1) (closes - opens - 1))
        in
        Queue.push (InputList parsed) items;
        i := closes + 2;
        j := !i
    | ',' ->
        let sub = String.sub line !j (!i - !j) in
        let x = Scanf.sscanf sub "%d" id in
        Queue.push (Number x) items;
        i := !i + 1;
        j := !i
    | _ -> i := !i + 1
  done;
  (if !j < String.length line then
   let sub = String.sub line !j (String.length line - !j) in
   let x = Scanf.sscanf sub "%d" id in
   Queue.push (Number x) items);
  items |> Queue.to_seq |> List.of_seq

let parse lines =
  lines
  |> Seq.group (fun a b -> b <> "")
  |> Seq.mapi (fun i lines -> if i = 0 then lines else lines |> Seq.drop 1)
  |> Seq.map (Seq.map (fun line -> line |> parse_line |> List.hd))
  |> Seq.map (fun group ->
         let[@warning "-8"] [ x; y ] = group |> List.of_seq in
         (x, y))

let const x y = x

let rec compare_items a b =
  match (a, b) with
  | Number a, Number b -> compare a b
  | Number a, InputList b ->
      compare_items (InputList [ Number a ]) (InputList b)
  | InputList a, Number b ->
      compare_items (InputList a) (InputList [ Number b ])
  | InputList a, InputList b ->
      let result =
        Seq.fold_left2
          (fun acc a b -> if acc = 0 then compare_items a b else acc)
          0 (List.to_seq a) (List.to_seq b)
      in
      if result = 0 then compare (List.length a) (List.length b) else result
(* let diff_a = *)
(*   if List.length b > List.length a then List.length b - List.length a *)
(*   else 0 *)
(* in *)
(* let diff_b = *)
(*   if List.length a > List.length b then List.length a - List.length b *)
(*   else 0 *)
(* in *)
(* let a = a @ List.init diff_a (const (Number Int.min_int)) in *)
(* let b = b @ List.init diff_b (const (Number Int.min_int)) in *)
(* List.fold_left2 *)
(*   (fun acc a b -> if acc = 0 then compare_items a b else acc) *)
(*   0 a b *)

let solve1 lines =
  parse lines
  |> Seq.mapi (fun i (a, b) -> if compare_items a b < 0 then i + 1 else 0)
  |> Seq.fold_left ( + ) 0
;;

(* parse lines |> Seq.map List.of_seq |> List.of_seq *)
parse lines |> List.of_seq;;
solve1 lines;;
Printf.printf "Part 1: %d\n" (solve1 input_lines)

let distress1 = InputList [ InputList [ Number 2 ] ]
let distress2 = InputList [ InputList [ Number 6 ] ]
let rec find a x n = if a.(n) = x then n else find a x (n + 1)

let rec to_string item =
  match item with
  | Number x -> Printf.sprintf "%d" x
  | InputList lst ->
      "[" ^ (lst |> List.map to_string |> String.concat ",") ^ "]"

let solve2 lines =
  let parsed =
    parse lines
    |> Seq.flat_map (fun (a, b) -> [ a; b ] |> List.to_seq)
    |> Seq.append ([ distress1; distress2 ] |> List.to_seq)
    |> Array.of_seq
  in
  Array.sort compare_items parsed;
  Printf.printf "Sorted Array:\n%s\n"
    (Array.to_seq parsed |> Seq.map to_string |> List.of_seq
   |> String.concat "\n");
  (* (find parsed distress1 0, find parsed distress2 0) *)
  parsed |> Array.to_seq
  |> Seq.mapi (fun i x -> if x = distress1 || x = distress2 then i + 1 else 1)
  |> Seq.fold_left (fun a b -> a * b) 1
;;

solve2 lines;;
Printf.printf "Part 2: %d\n" (solve2 input_lines)
(* let a, b = solve2 input_lines in *)
(* Printf.printf "Part 2: %d, %d\n" a b *)

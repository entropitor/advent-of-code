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
  "root: pppw + sjmn\n\
   dbpl: 5\n\
   cczh: sllz + lgvd\n\
   zczc: 2\n\
   ptdq: humn - dvpt\n\
   dvpt: 3\n\
   lfqf: 4\n\
   humn: 5\n\
   ljgn: 2\n\
   sjmn: drzm * dbpl\n\
   sllz: 4\n\
   pppw: cczh / lfqf\n\
   lgvd: ljgn * ptdq\n\
   drzm: hmdt - zczc\n\
   hmdt: 32" |> String.split_on_char '\n' |> List.to_seq

let input_lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

type expr =
  | Number of int
  | Add of string * string
  | Subtract of string * string
  | Multiply of string * string
  | Division of string * string

let parse_expr expr =
  try Number (int_of_string expr)
  with Failure _ -> (
    match Scanf.sscanf expr "%s %c %s" (fun a b c -> (a, b, c)) with
    | a, '+', b -> Add (a, b)
    | a, '-', b -> Subtract (a, b)
    | a, '*', b -> Multiply (a, b)
    | a, '/', b -> Division (a, b)
    | _ -> failwith "Unexpected operation")

let parse lines =
  lines
  |> Seq.map (fun line ->
         Scanf.sscanf line "%s@: %s@\n" (fun name expr ->
             (name, parse_expr expr)))
  |> Hashtbl.of_seq

let solve1 lines =
  let tbl = parse lines in
  let solved_tbl = Hashtbl.create (Hashtbl.length tbl) in
  let rec solve name =
    match Hashtbl.find_opt solved_tbl name with
    | None ->
        let score =
          let expr = Hashtbl.find tbl name in
          match expr with
          | Number x -> x
          | Add (a, b) -> solve a + solve b
          | Subtract (a, b) -> solve a - solve b
          | Multiply (a, b) -> solve a * solve b
          | Division (a, b) -> solve a / solve b
        in
        Hashtbl.replace solved_tbl name score;
        score
    | Some x -> x
  in
  solve "root"

type lazy_op = Add | Sub | Rev_Sub | Mul | Div | Rev_Div
type lazy_value = Fixed of int | Op of lazy_op * int * lazy_value | Human

let apply a b f op rev_op =
  match (a, b) with
  | Fixed a, Fixed b -> Fixed (f a b)
  | Fixed a, b -> Op (op, a, b)
  | a, Fixed b -> Op (rev_op, b, a)
  | _ -> failwith "both sides unknown"

let print_lazy_op a =
  match a with
  | Add -> "Add"
  | Sub -> "Sub"
  | Rev_Sub -> "Rev_Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Rev_Div -> "Rev_Div"

let rec print_lazy_value a =
  match a with
  | Fixed v -> Printf.sprintf "Fixed (%d)" v
  | Human -> "Human"
  | Op (op, value, lazy_value) ->
      Printf.sprintf "Op (%d %s (%s))" value (print_lazy_op op)
        (print_lazy_value lazy_value)

let solve2 lines =
  let tbl = parse lines in
  let solved_tbl = Hashtbl.create (Hashtbl.length tbl) in
  let rec solve name =
    match Hashtbl.find_opt solved_tbl name with
    | None ->
        let score =
          if name = "humn" then Human
          else
            let expr = Hashtbl.find tbl name in
            match expr with
            | Number x -> Fixed x
            | Add (a, b) -> apply (solve a) (solve b) ( + ) Add Add
            | Subtract (a, b) -> apply (solve a) (solve b) ( - ) Sub Rev_Sub
            | Multiply (a, b) -> apply (solve a) (solve b) ( * ) Mul Mul
            | Division (a, b) -> apply (solve a) (solve b) ( / ) Div Rev_Div
        in
        Hashtbl.replace solved_tbl name score;
        score
    | Some score -> score
  in
  let a, b =
    match Hashtbl.find tbl "root" with
    | Add (a, b) -> (a, b)
    | _ -> failwith "root is not addition"
  in
  let rec compute a b =
    let apply value op value2 =
      match op with
      | Add -> value - value2
      | Sub -> value2 - value
      | Rev_Sub -> value + value2
      | Mul -> value / value2
      | Div -> value2 / value
      | Rev_Div -> value * value2
    in
    match (a, b) with
    | Op _, Fixed _ | Human, Fixed _ -> compute b a
    | Fixed value, Op (op, value2, next) ->
        compute (Fixed (apply value op value2)) next
    | Fixed value, Human -> value
    | _, _ -> failwith "unexpected"
  in
  compute (solve a) (solve b)
;;

Printf.printf "Part 1 (example): %d\n" (solve1 lines);;
flush stdout;;
Printf.printf "Part 1: %d\n\n" (solve1 input_lines);;
flush stdout;;
Printf.printf "Part 2 (example): %d\n" (solve2 lines);;
flush stdout;;
Printf.printf "Part 2: %d\n" (solve2 input_lines);;
flush stdout

open System

type Result =
  | Found of int
  (* Key: total so far  + nb values used for it
     Value: product of those numbers *)
  | NotFound of Map<int * int, int>
let join (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])
let main goal amountOfNumbers lines =
  let folder state next =
    match state with
    | Found _ -> state
    | NotFound seen -> 
      let key = (goal - next, amountOfNumbers - 1)
      match Map.tryFind key seen with
      | Some value -> Found (next * value)
      | None -> NotFound (seen
          |> join (seq {
            for (total, nbUsed), value in Map.toSeq seen do
            if nbUsed + 1 < amountOfNumbers then
              yield ((total + next, nbUsed + 1), value * next)
          } |> Map)
          |> Map.add (next, 1) next
      )
  let result = lines |> Seq.fold folder (NotFound Map.empty)
  result

let testInput = seq {
  1721
  979
  366
  299
  675
  1456
}
(* A *)
main 2020 2 testInput = Found 514579
(* B *)
main 2020 3 testInput = Found 241861950

(* Real *)
let read _ = Console.ReadLine()
let isValid x = x <> null
let inputLines = Seq.initInfinite read |> Seq.takeWhile isValid
let realInput = inputLines |> Seq.map int

(* A *)
main 2020 2 realInput |> printf "%A"
(* B *)
main 2020 3 realInput |> printf "%A"

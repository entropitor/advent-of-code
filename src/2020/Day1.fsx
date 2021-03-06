#load "shared.fsx"

open Shared
open System

let testInput =
    seq {
        1721
        979
        366
        299
        675
        1456
    }

let parse = int

type Result =
    | Found of int
    (* Key: total so far  + nb values used for it
     Value: product of those numbers *)
    | NotFound of Map<int * int, int>

let join (p: Map<'a, 'b>) (q: Map<'a, 'b>) =
    Map
        (Seq.concat [ (Map.toSeq p)
                      (Map.toSeq q) ])

let main goal amountOfNumbers lines =
    let folder state next =
        match state with
        | Found _ -> state
        | NotFound seen ->
            let key = (goal - next, amountOfNumbers - 1)

            match Map.tryFind key seen with
            | Some value -> Found(next * value)
            | None ->
                NotFound
                    (seen
                     |> join
                         (seq {
                             for (total, nbUsed), value in Map.toSeq seen do
                                 if nbUsed + 1 < amountOfNumbers
                                 then yield ((total + next, nbUsed + 1), value * next)
                          }
                          |> Map)
                     |> Map.add (next, 1) next)

    let result =
        lines |> Seq.fold folder (NotFound Map.empty)

    result

let realInput = inputLines |> Seq.map parse

(* A *)
assertEqual (testInput |> main 2020 2) (Found 514579)

main 2020 2 realInput |> printfn "%A"
(* B *)
assertEqual (testInput |> main 2020 3) (Found 241861950)

main 2020 3 realInput |> printfn "%A"

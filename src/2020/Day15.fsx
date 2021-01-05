#load "shared.fsx"

open Shared
open System

let lines = split "0,3,6"

let parse lines =
    lines
    |> Seq.head
    |> (fun (line: string) -> line.Split(","))
    |> Seq.map int

let finishSequence count start =
    let startMinusLast = start |> Seq.take (Seq.length start - 1)
    let flip a b = (b, a)

    let initialPositions =
        let positions = Array.init count (fun _ -> -1)

        startMinusLast
        |> Seq.iteri (fun index number -> positions.[number] <- index)

        positions

    let lastSeen item index (lastPositions: int array) =
        let last = lastPositions.[item]
        if last = -1 then 0 else index - last

    let folder (lastPositions, lastItem) index =
        let next = lastSeen lastItem index lastPositions
        lastPositions.[lastItem] <- index
        (lastPositions, next)

    let nbBefore = Seq.length startMinusLast

    Seq.append
        (startMinusLast)
        (Seq.initInfinite (fun i -> i + nbBefore)
         |> Seq.scan folder (initialPositions, Seq.last start)
         |> Seq.map snd)

let main count lines =
    lines
    |> parse
    |> finishSequence count
    |> Seq.item (count - 1)

assertEqual
    [| 0; 3; 6; 0; 3; 3; 1; 0; 4; 0 |]
    (lines
     |> parse
     |> finishSequence 100
     |> Seq.take 10
     |> Seq.toArray)

assertEqual 436 (lines |> main 2020)

inputLines |> main 2020 |> printfn "%A"

(* assertEqual 175594 (lines |> main 30000000) *)
inputLines |> main 30000000 |> printfn "%A"

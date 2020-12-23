#load "shared.fsx"

open Shared
open System

let bsp chr (min, max) next =
    if chr = next then min + (max - min) / 2 + 1, max else min, min + (max - min) / 2

let row line =
    line
    |> Seq.take 7
    |> Seq.fold (bsp 'B') (0, 127)
    |> fst

let column line =
    line
    |> Seq.skip 7
    |> Seq.fold (bsp 'R') (0, 7)
    |> fst

let seat line = (row line) * 8 + (column line)

let maxSeat lines = lines |> Seq.map seat |> Seq.max

let missingSeat lines =
    let seats = lines |> Seq.map seat |> Seq.sort

    let zipped =
        seats
        |> Seq.append (seats |> Seq.head |> Seq.singleton)
        |> Seq.zip seats

    let before =
        zipped
        |> Seq.skipWhile (fun (y, x) -> y - x <> 2)
        |> Seq.head
        |> snd

    assert false
    before + 1

let test =
    """BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL"""
        .Split("\n")
    |> Seq.ofArray
(*
test |> Seq.map row
test |> Seq.map column
test |> Seq.map seat
*)


(* A *)
test |> maxSeat
(* inputLines |> maxSeat |> printfn "%A" *)

(* B *)
test |> missingSeat
(* inputLines |> missingSeat |> printfn "%A" *)

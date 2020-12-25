#load "shared.fsx"

open Shared
open System

let lines =
    let input = """abc

a
b
c

ab
ac

a
a
a
a

b"""
    input.Split(" ") |> Seq.ofArray

let main folder lines =
    let groups = (String.concat "\n" lines).Split("\n\n")

    let split (str: String) = str.Split("\n")

    groups
    |> Seq.map
        (split
         >> Seq.map Set
         >> fun sq -> Seq.fold folder (Seq.head sq) sq
         >> Set.count)
    |> Seq.sum

(* A *)
assertEqual 11 (lines |> main Set.union)

inputLines |> main Set.union |> printfn "%A"

assertEqual 6 (lines |> main Set.intersect)
inputLines |> main Set.intersect |> printfn "%A"

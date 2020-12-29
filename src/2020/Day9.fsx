#load "shared.fsx"

open Shared
open System

let lines =
    let input = """35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"""
    input.Split("\n")

let parse (lines: string seq) = Seq.map int64 lines |> List.ofSeq

let invalidNumber preambleSize (numbers: int64 list) =
    numbers
    |> List.skip preambleSize
    |> List.indexed
    |> List.find (fun (i, number) ->
        let slice = numbers.[i..i + preambleSize - 1]

        List.allPairs slice slice
        |> List.map (fun (a, b) -> a + b)
        |> List.contains number
        |> not)
    |> snd

let rangeThatSumsTo sum (numbers: int64 list) =
    let folder a b = a + b

    let (index, sums) =
        numbers
        |> Seq.indexed
        |> Seq.map (fun (i, _number) ->
            let sums =
                numbers
                |> Seq.skip i
                |> Seq.scan folder 0L
                |> Seq.skip 1
                |> Seq.takeWhile (fun x -> x <= sum)

            if Seq.last sums = sum then Some(i, sums) else None)
        |> Seq.find Option.isSome
        |> Option.get

    let range =
        numbers.[index..index + Seq.length sums - 1]

    Seq.min range + Seq.max range

let mainSimple lines = lines |> parse |> invalidNumber 25

let mainComplex lines =
    let numbers = lines |> parse
    let invalidNumber = invalidNumber 25 numbers
    rangeThatSumsTo invalidNumber numbers

(* A *)
assertEqual 127L (lines |> parse |> invalidNumber 5)

inputLines |> mainSimple |> printfn "%A"

(* A *)
assertEqual 62L (lines |> parse |> rangeThatSumsTo 127L)

inputLines |> mainComplex |> printfn "%A"

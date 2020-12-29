#load "shared.fsx"

open Shared
open System

let lines =
    let input = """16
10
15
5
1
11
7
19
6
12
4"""
    input.Split("\n")

let mainSimple lines =
    let numbers = Seq.map int lines
    let sorted = Seq.sort numbers

    let sorted =
        Seq.append sorted (Seq.singleton ((Seq.last sorted) + 3))

    let counts =
        Seq.zip (Seq.append (Seq.singleton 0) sorted) sorted
        |> Seq.map (fun (a, b) -> b - a)
        |> Seq.groupBy id
        |> Seq.map (fun (group, counts) -> (group, Seq.length counts))
        |> Map

    counts.[1] * counts.[3]

let splitBy f input =
    let i = ref 0

    input
    |> Seq.map (fun x ->
        if f x then incr i
        !i, x)
    |> Seq.groupBy fst
    |> Seq.map (fun (_, b) -> Seq.map snd b)


let mainComplex lines =
    let numbers = Seq.map int lines
    let sorted = Seq.sort numbers

    let sorted =
        Seq.append sorted (Seq.singleton ((Seq.last sorted) + 3))

    let differences =
        Seq.zip (Seq.append (Seq.singleton 0) sorted) sorted
        |> Seq.map (fun (a, b) -> b - a)
        |> splitBy (fun x -> x = 3)
        |> Seq.map (Seq.filter (fun x -> x <> 3))

    printfn "%A" (Seq.toList differences)

    differences
    |> Seq.map Seq.length
    |> Seq.map (function
        | 4 -> 7L
        | 3 -> 4L
        | 2 -> 2L
        | 1 -> 1L
        | 0 -> 1L
        | x -> failwith (sprintf "%d" x))
    |> Seq.fold (fun a b -> a * b) 1L

(* A *)
inputLines |> mainSimple |> printfn "%A"

(* B *)
inputLines |> mainComplex |> printfn "%A"

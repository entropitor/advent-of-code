#load "shared.fsx"

open Shared
open System

let lines =
    let input = """939
7,13,x,x,59,x,31,19"""
    input.Split("\n")

let parseSimple (lines: string seq) =
    let earlieast = Seq.head lines |> int

    let buses =
        (Seq.item 1 lines).Split(",")
        |> Seq.filter (fun bus -> bus <> "x")
        |> Seq.map int

    earlieast, buses

let bestBus (earliest, buses) =
    buses
    |> Seq.map (fun bus ->
        let last = earliest / bus
        bus, bus * (last + 1))
    |> Seq.minBy snd

let mainSimple lines =
    let parsed = parseSimple lines
    let (bus, arrivesAt) = bestBus parsed
    bus * (arrivesAt - fst parsed)

let rec gcd a b =
    if b > a then
        let (s, t) = gcd b a
        (t, s)
    else
        let folder (r, s, t) _ =
            let q = fst r / snd r
            let next x = (snd x, fst x - q * snd x)
            (next r, next s, next t)

        Seq.initInfinite id
        |> Seq.scan folder ((a, b), (1I, 0I), (0I, 1I))
        |> Seq.takeWhile (fun (r, _, _) -> snd r <> 0I)
        |> Seq.last
        |> fun (_, s, t) -> snd s, snd t

let parseComplex (lines: string seq) =
    (Seq.item 1 lines).Split(",")
    |> Seq.indexed
    |> Seq.filter (fun bus -> snd bus <> "x")
    |> Seq.map (fun (i, bus) -> (bigint -i, int bus |> bigint))

let inverse a b =
    let (s, _) = gcd a b
    (* s * a + t * b = 1 *)
    let x = s % b

    if x < 0I then b + x else x


let solveComplex buses =
    let N = Seq.fold (fun a b -> a * snd b) 1I buses

    let mapper (a1, n1) =
        let n2 = N / n1
        let m2 = inverse n2 n1

        a1 * m2 * n2

    let r = buses |> Seq.map mapper |> Seq.sum

    let result = r % N
    if result < 0I then result + N else result

let mainComplex lines =
    let buses = parseComplex lines
    solveComplex buses

let testComplex value line =
    assertEqual
        (value)
        (mainComplex
            (seq {
                yield ""
                yield line
             }))

(* A *)
assertEqual (59, 944) (lines |> parseSimple |> bestBus)

assertEqual 295 (lines |> mainSimple)
inputLines |> mainSimple |> printfn "%A"

(* B *)
assertEqual 1068781I (mainComplex lines)

testComplex 3417I "17,x,13,19"
testComplex 754018I "67,7,59,61"
testComplex 779210I "67,x,7,59,61"

testComplex 1261476I "67,7,x,59,61"
inputLines |> mainComplex |> printfn "%A"

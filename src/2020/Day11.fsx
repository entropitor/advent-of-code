#load "shared.fsx"

open Shared
open System

let lines =
    let input = """L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"""
    input.Split("\n")

type Seat =
    | Empty
    | Floor
    | Occupied

let parse lines =
    let parseChar =
        function
        | 'L' -> Empty
        | '.' -> Floor
        | '#' -> Occupied
        | _ -> failwith "parse error"

    let parseLine line = line |> Seq.map parseChar |> Array.ofSeq

    lines |> Seq.map parseLine |> Array.ofSeq

let afterOne =
    let input = """#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##"""
    let lines = input.Split("\n")
    parse lines

let fixedPoint =
    let input = """#.#L.L#.##
#LLL#LL.L#
L.#.L..#..
#L##.##.L#
#.#L.LL.LL
#.#L#L#.##
..L.L.....
#L#L##L#L#
#.LLLLLL.L
#.#L#L#.##"""
    let lines = input.Split("\n")
    parse lines

let count rowNumber columnNumber (map: Seat [] []) =
    let nbRows = Array.length map
    let nbColumns = Array.length map.[0]

    seq {
        if rowNumber > 0 then
            if columnNumber > 0
            then yield map.[rowNumber - 1].[columnNumber - 1]

            yield map.[rowNumber - 1].[columnNumber]

            if columnNumber + 1 < nbColumns
            then yield map.[rowNumber - 1].[columnNumber + 1]

        if columnNumber > 0 then yield map.[rowNumber].[columnNumber - 1]
        if columnNumber + 1 < nbColumns then yield map.[rowNumber].[columnNumber + 1]

        if rowNumber + 1 < nbRows then
            if columnNumber > 0
            then yield map.[rowNumber + 1].[columnNumber - 1]

            yield map.[rowNumber + 1].[columnNumber]

            if columnNumber + 1 < nbColumns
            then yield map.[rowNumber + 1].[columnNumber + 1]
    }
    |> Seq.filter (fun x -> x = Occupied)
    |> Seq.length

let evolveOne map =
    map
    |> Array.mapi (fun rowNumber row ->
        row
        |> Array.mapi (fun columnNumber cell ->
            let count = count rowNumber columnNumber map

            match cell with
            | Floor -> Floor
            | Empty -> if count = 0 then Occupied else Empty
            | Occupied -> if count >= 4 then Empty else Occupied))

let evolveUntilFixedPoint lines =
    let map = parse lines

    let rec loop map =
        let newMap = evolveOne map
        if newMap = map then map else loop newMap

    loop map

let countOccupied map =
    let isOccupied x = if x = Occupied then 1 else 0

    map
    |> Array.map (Array.map isOccupied >> Seq.sum)
    |> Seq.sum


assertEqual afterOne (evolveOne (parse lines))
assertEqual fixedPoint (evolveUntilFixedPoint lines)

inputLines
|> evolveUntilFixedPoint
|> countOccupied
|> printfn "%A"

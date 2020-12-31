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

type Cell =
    { Seat: Seat
      Neighbours: (int * int) list }

let findSeatCoordinates row column (map: Seat [,]) =
    let nbRows = Array2D.length1 map
    let nbColumns = Array2D.length2 map

    let isValidCoordinate (r, c) =
        r >= 0 && r < nbRows && c >= 0 && c < nbColumns

    let findCoordinate direction =
        let coordinate n =
            (row + n * fst direction, column + n * snd direction)

        [ 1 .. (max nbRows nbColumns) ]
        |> Seq.map coordinate
        |> Seq.filter isValidCoordinate
        |> Seq.tryFind (fun x -> Array2D.get map (fst x) (snd x) <> Floor)

    seq {
        findCoordinate (-1, -1)
        findCoordinate (-1, 0)
        findCoordinate (-1, 1)
        findCoordinate (0, -1)
        findCoordinate (0, 1)
        findCoordinate (1, -1)
        findCoordinate (1, 0)
        findCoordinate (1, 1)
    }
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    |> List.ofSeq

let parse lines =
    let parseChar =
        function
        | 'L' -> Empty
        | '.' -> Floor
        | '#' -> Occupied
        | c -> failwith (sprintf "parse error char %A" c)

    let parseLine line = line |> Seq.map parseChar

    let arrayOfSeats = lines |> Seq.map parseLine |> array2D

    arrayOfSeats
    |> Array2D.mapi (fun row column cell ->
        let neighbours =
            findSeatCoordinates row column arrayOfSeats

        { Seat = cell; Neighbours = neighbours })

let count rowNumber columnNumber (map: Cell [,]) =
    let cell = map.[rowNumber, columnNumber]

    cell.Neighbours
    |> Seq.map (fun coord -> map.[fst coord, snd coord])
    |> Seq.filter (fun x -> x.Seat = Occupied)
    |> Seq.length

let evolveOne map =
    map
    |> Array2D.mapi (fun row column cell ->
        let count = count row column map

        match cell.Seat with
        | Floor -> { cell with Seat = Floor }
        | Empty -> if count = 0 then { cell with Seat = Occupied } else cell
        | Occupied -> if count >= 5 then { cell with Seat = Empty } else cell)

let evolveUntilFixedPoint map =
    let rec loop map =
        let newMap = evolveOne map
        if newMap = map then map else loop newMap

    loop map

let countOccupied map =
    let count = ref 0
    Array2D.iter (fun x -> if x.Seat = Occupied then incr count) map
    !count


assertEqual
    26
    (lines
     |> parse
     |> evolveUntilFixedPoint
     |> countOccupied)

inputLines
|> parse
|> evolveUntilFixedPoint
|> countOccupied
|> printfn "%A"

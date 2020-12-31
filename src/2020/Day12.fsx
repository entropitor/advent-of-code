#load "shared.fsx"

open Shared
open System

let lines =
    let input = """F10
N3
F7
R90
F11"""
    input.Split("\n")

let parse (line: string) = (line.[0], line.[1..] |> int)

let rec rotate amount waypoint =
    if amount = 0 then
        waypoint
    else
        let amount = amount % 360

        let rotateOne =
            function
            | (wx, wy) -> (wy, -wx)

        rotate (amount - 90) (rotateOne waypoint)

let main lines =
    let folder (x, y, dir) (command, amount) =
        match command with
        | 'N' -> (x, y + amount, dir)
        | 'S' -> (x, y - amount, dir)
        | 'E' -> (x + amount, y, dir)
        | 'W' -> (x - amount, y, dir)
        | 'R' -> (x, y, rotate amount dir)
        | 'L' -> (x, y, rotate (360 - amount) dir)
        | 'F' -> (x + amount * fst dir, y + amount * snd dir, dir)
        | _ -> (x, y, dir)

    let (x, y, _dir) =
        lines
        |> Seq.map parse
        |> Seq.fold folder (0, 0, (1, 0))

    Math.Abs(x) + Math.Abs(y)

let mainComplex lines =
    let folder ((sx, sy), (wx, wy)) (command, amount) =
        match command with
        | 'N' -> ((sx, sy), (wx, wy + amount))
        | 'S' -> ((sx, sy), (wx, wy - amount))
        | 'E' -> ((sx, sy), (wx + amount, wy))
        | 'W' -> ((sx, sy), (wx - amount, wy))
        | 'R' -> ((sx, sy), rotate amount (wx, wy))
        | 'L' -> ((sx, sy), rotate (360 - amount) (wx, wy))
        | 'F' -> ((sx + amount * wx, sy + amount * wy), (wx, wy))
        | _ -> ((sx, sy), (wx, wy))

    let ((x, y), _waypoint) =
        lines
        |> Seq.map parse
        |> Seq.fold folder ((0, 0), (10, 1))

    Math.Abs(x) + Math.Abs(y)

(* A *)
assertEqual 25 (main lines)

main inputLines |> printfn "%A"

(* B *)
assertEqual 286 (mainComplex lines)

mainComplex inputLines |> printfn "%A"

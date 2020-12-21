#load "shared.fsx"
open Shared
open System

let testInput = seq {
  "..##......."
  "#...#...#.."
  ".#....#..#."
  "..#.#...#.#"
  ".#...##..#."
  "..#.##....."
  ".#.#.#....#"
  ".#........#"
  "#.##...#..."
  "#...##....#"
  ".#..#...#.#"
}

let mainSimple right lines =
  let folder index line =
    let length = line |> Seq.length
    let index = index % length
    let character = line |> Seq.item index
    (character, index + right)
  lines
    |> Seq.mapFold folder 0
    |> fst
    |> Seq.filter (fun x -> x = '#')
    |> Seq.length

let mainComplex lines =
  let simpleSlopes = seq { 1; 3; 5; 7 } |> Seq.map (fun x -> mainSimple x lines |> int64) |> Seq.fold (fun x y -> x * y) 1L
  let even i = i % 2 = 0
  let complexSlope = mainSimple 1 (lines |> Seq.indexed |> Seq.filter (fst >> even) |> Seq.map snd) |> int64
  simpleSlopes * complexSlope

(* A *)
assertEqual (testInput |> mainSimple 3) 7
inputLines |> mainSimple 3 |> printfn "%A"
(* B *)
assertEqual (testInput |> mainComplex) 336L
inputLines |> mainComplex |> printfn "%A"


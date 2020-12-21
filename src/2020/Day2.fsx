#load "shared.fsx"
open Shared
open System

type Parsed = {
  Min: int;
  Max: int;
  Letter: char;
  Input: String;
}
let parse (str: String) = 
  let parts = str.Replace(":", "").Split(" ")
  let amounts = parts.[0].Split("-")
  {
    Min = amounts.[0] |> int;
    Max = amounts.[1] |> int;
    Letter = parts.[1] |> Seq.head;
    Input = parts.[2];
  }
let testInput = seq {
  "1-3 a: abcde"
  "1-3 b: cdefg"
  "2-9 c: ccccccccc"
}

let check (parsed: Parsed) =
  let isLetter x = x = parsed.Letter
  let count = parsed.Input |> Seq.filter isLetter |> Seq.length
  parsed.Min <= count && count <= parsed.Max

let checkB (parsed: Parsed) =
  let isLetter x = x = parsed.Letter
  let first = parsed.Input.[parsed.Min - 1]
  let second = parsed.Input.[parsed.Max - 1]
  first <> second && (isLetter first || isLetter second)

let main check = Seq.map parse >> Seq.filter check >> Seq.length

testInput |> main check
testInput |> main checkB

(* A *)
(* inputLines |> main check |> printfn "%A" *)
(* B *)
inputLines |> main checkB |> printfn "%A"

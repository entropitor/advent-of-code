#load "shared.fsx"

open Shared
open System

let lines =
    let input = """nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"""
    input.Split("\n")

type Operation =
    | Acc of int
    | Jump of int
    | Nop of int

let parse lines =
    let parse (line: string) =
        let parts = line.Split(" ")
        let x = int parts.[1]

        match parts.[0] with
        | "acc" -> Acc x
        | "jmp" -> Jump x
        | "nop" -> Nop x
        | _ -> failwith "unknown"

    lines |> Seq.map parse

let evaluate pc acc instruction =
    match instruction with
    | Acc diffAcc -> (pc + 1, acc + diffAcc)
    | Jump diffPc -> (pc + diffPc, acc)
    | Nop _ -> (pc + 1, acc)

type ProgramResult =
    | Loop of int
    | Ended of int

let detectLoop pc acc program =
    let length = Seq.length program

    let rec loop seenPcs pc acc =
        let instruction = Seq.item pc program
        let (pc, acc) = evaluate pc acc instruction

        if Set.contains pc seenPcs then Loop acc
        else if pc = length then Ended acc
        else loop (Set.add pc seenPcs) pc acc

    if pc = length then Ended acc else loop Set.empty pc acc

let mainSimple lines = lines |> parse |> detectLoop 0 0

let isFixable =
    function
    | Acc _ -> false
    | Nop _ -> true
    | Jump _ -> true

let fixInstruction =
    function
    | Acc x -> Acc x
    | Jump _ -> Nop 0
    | Nop x -> Jump x


let mainComplex lines =
    let program = parse lines

    let rec loop (pc, acc) =
        let instruction = Seq.item pc program

        let otherwise () = evaluate pc acc instruction |> loop

        if isFixable instruction then
            let (pc, acc) =
                evaluate pc acc (fixInstruction instruction)

            match detectLoop pc acc program with
            | Ended acc -> acc
            | Loop _ -> otherwise ()
        else
            otherwise ()

    loop (0, 0)

(* let safe program = *)
(*     let length = Seq.length program *)

(*     let newPcs = *)
(*         program *)
(*         |> Seq.mapi (fun i instruction -> evaluate i 0 instruction |> fst) *)
(*         |> Array.ofSeq *)

(*     let rec safePositions safe = *)
(*         let safe' = *)
(*             safe *)
(*             |> Array.map (fun newPc -> *)
(*                 if newPc = length || Array.item newPc safe = length *)
(*                 then length *)
(*                 else newPc) *)

(*         if safe' = safe then safe else safePositions safe' *)

(*     safePositions newPcs *)
(*     |> Array.map (fun x -> x = length) *)

(* let accumulatorBeforeFixedEnd lines = *)
(*     let program = parse lines *)
(*     let safe = safe program *)
(*     let length = Seq.length program *)

(*     let rec run changed pc acc = *)

(*         if changed && pc = length then *)
(*             acc *)
(*         else *)
(*             let instruction = Seq.item pc program *)

(*             if changed then *)
(*                 let (pc, acc) = evaluate pc acc instruction *)
(*                 run true pc acc *)
(*             else *)
(*                 let (newPc, newAcc) = *)
(*                     evaluate pc acc (fixInstruction instruction) *)

(*                 if Array.item newPc safe then *)
(*                     run true newPc newAcc *)
(*                 else *)
(*                     let (pc, acc) = evaluate pc acc (Seq.item pc program) *)

(*                     run false pc acc *)

(*     run false 0 0 *)


(* A *)
assertEqual (Loop 5) (mainSimple lines)

inputLines |> mainSimple |> printfn "%A"

(* B *)
(* assertEqual 8 (accumulatorBeforeFixedEnd lines) *)

(* accumulatorBeforeFixedEnd inputLines *)
(* |> printfn "%A" *)

assertEqual 8 (mainComplex lines)

mainComplex inputLines |> printfn "%A"

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

let evaluate pc acc instruction count =
    let (pc, acc) =
        match instruction with
        | Acc diffAcc -> (pc + 1, acc + diffAcc)
        | Jump diffPc -> (pc + diffPc, acc)
        | Nop _ -> (pc + 1, acc)

    (pc % count, acc)

let accumulatorBeforeRepeat lines =
    let program = parse lines

    let rec loop seenPcs pc acc =
        let (pc, acc) =
            evaluate pc acc (Seq.item pc program) (Seq.length program)

        if Set.contains pc seenPcs then acc else loop (Set.add pc seenPcs) pc acc

    loop Set.empty 0 0

let safe program =
    let newPcs =
        program
        |> Seq.mapi (fun i instruction ->
            evaluate i 0 instruction (Seq.length program)
            |> fst)
        |> Array.ofSeq

    let rec safePositions safe =
        let safe' =
            safe
            |> Array.map (fun newPc -> if Array.item newPc safe = 0 then 0 else newPc)

        if safe' = safe then safe else safePositions safe'

    safePositions newPcs |> Array.map (fun x -> x = 0)

let fixInstruction =
    function
    | Acc x -> Acc x
    | Jump _ -> Nop 0
    | Nop x -> Jump x

let accumulatorBeforeFixedEnd lines =
    let program = parse lines
    let safe = safe program
    let length = Seq.length program

    let rec run changed pc acc =
        let instruction = Seq.item pc program

        if changed then
            if pc = 0 then
                acc
            else
                let (pc, acc) = evaluate pc acc instruction length
                run true pc acc
        else
            let (newPc, newAcc) =
                evaluate pc acc (fixInstruction instruction) length

            if Array.item newPc safe then
                run true newPc newAcc
            else
                let (pc, acc) =
                    evaluate pc acc (Seq.item pc program) (Seq.length program)

                run false pc acc

    run false 0 0


(* A *)
assertEqual 5 (accumulatorBeforeRepeat lines)

accumulatorBeforeRepeat inputLines |> printfn "%A"

(* B *)
assertEqual 8 (accumulatorBeforeFixedEnd lines)

accumulatorBeforeFixedEnd inputLines
|> printfn "%A"

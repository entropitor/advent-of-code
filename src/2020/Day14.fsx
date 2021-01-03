#load "shared.fsx"

open Shared
open System

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let lines =
    let input = """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"""
    input.Split("\n")

type MaskBit =
    | SetOne
    | SetZero
    | Passthrough

type Instruction =
    | SetMemory of int64 * int64
    | SetMask of MaskBit array

let parse lines =
    let parseMask mask =
        mask
        |> Seq.map (function
            | 'X' -> Passthrough
            | '1' -> SetOne
            | '0' -> SetZero
            | _ -> failwith "Unknown mask bit")
        |> Seq.rev
        |> Array.ofSeq

    let parse (line: string) =
        match line with
        | Regex "mem\[(\d+)\] = (\d+)" [ address; number ] -> SetMemory(int64 address, int64 number)
        | Regex "mask = ([01X]+)" [ newMask ] -> SetMask(parseMask newMask)
        | _ -> failwith (sprintf "unknown line: %A" line)

    Seq.map parse lines

let executeProgram program =
    let toBits number =
        Seq.initInfinite id
        |> Seq.map (fun i -> (number / (pown 2L i)) % 2L)
        |> Seq.take 36

    let fromBits bits =
        bits
        |> Seq.mapi (fun i bit -> bit * (pown 2L i))
        |> Seq.sum

    let masked value mask =
        toBits value
        |> Seq.zip mask
        |> Seq.map (function
            | (Passthrough, bit) -> bit
            | (SetZero, _) -> 0L
            | (SetOne, _) -> 1L)
        |> fromBits

    let initialMask =
        Seq.init 36 (fun _ -> Passthrough) |> Array.ofSeq

    let executeInstruction (memory, mask) instruction =
        match instruction with
        | SetMemory (address, value) ->

            let newMemory =
                memory |> Map.add address (masked value mask)

            (newMemory, mask)
        | SetMask newMask -> (memory, newMask)

    let (memory, _mask) =
        Seq.fold executeInstruction (Map.empty, initialMask) program

    memory

type Bit =
    | Floating
    | Bit of int64

let executeProgramV2 program =
    let toBits number =
        Seq.initInfinite id
        |> Seq.map (fun i -> (number / (pown 2L i)) % 2L)
        |> Seq.take 36

    let fromBits bits =
        let folder seqA seqB =
            Seq.allPairs seqA seqB
            |> Seq.map (fun (a, b) -> a + b)

        bits
        |> Seq.mapi (fun i bit ->
            let power = pown 2L i

            match bit with
            | Floating ->
                seq {
                    yield 0L
                    yield power
                }
            | Bit bit -> Seq.singleton (bit * power))
        |> Seq.fold folder (Seq.singleton 0L)

    let masked value mask =
        toBits value
        |> Seq.zip mask
        |> Seq.map (function
            | (Passthrough, _) -> Floating
            | (SetZero, bit) -> Bit bit
            | (SetOne, _) -> Bit 1L)
        |> fromBits

    let initialMask =
        Seq.init 36 (fun _ -> Passthrough) |> Array.ofSeq

    let executeInstruction (memory, mask) instruction =
        match instruction with
        | SetMemory (address, value) ->

            let addresses = masked address mask

            (* printfn "%A" (addresses) *)
            (* printfn "%A" (addresses |> Seq.length) *)

            let newMemory =
                Seq.concat [ (Map.toSeq memory)
                             (addresses
                              |> Seq.map (fun address -> (address, value))) ]
                |> Map.ofSeq

            (newMemory, mask)
        | SetMask newMask -> (memory, newMask)

    let (memory, _mask) =
        Seq.fold executeInstruction (Map.empty, initialMask) program

    memory

let sumMemory (memory: Map<int64, int64>) =
    memory
    |> Map.toSeq
    |> Seq.map (fun (_, value) -> value)
    |> Seq.sum

let mainSimple lines =
    lines |> parse |> executeProgram |> sumMemory

let mainComplex lines =
    lines |> parse |> executeProgramV2 |> sumMemory

(* A *)
assertEqual 165L (mainSimple lines)

inputLines |> mainSimple |> printfn "%A"

(* B *)
let lines2 =
    let input = """mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"""
    input.Split("\n")

assertEqual 208L (mainComplex lines2)
inputLines |> mainComplex |> printfn "%A"

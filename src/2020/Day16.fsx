#load "shared.fsx"
open Shared
open System

let lines =
  let input = """class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12"""
  input.Split("\n")

lines

open System.Text.RegularExpressions

let parse lines =
    let parseTicket (line: string) = line.Split(",") |> Seq.map int
    let parseGroup (line: string) =
        let parts = line.Split(": ")
        let ranges = parts.[1].Split(" or ")
        let parseRange (range: string) = 
            let parts = range.Split("-")
            int parts.[0], int parts.[1]
        parts.[0], ranges |> Seq.map parseRange

    let groups = lines |> Seq.takeWhile (fun x -> x <> "") |> Seq.map parseGroup
    let ticket = lines |> Seq.skip (Seq.length groups + 2) |> Seq.head |> parseTicket
    let otherTickets = lines |> Seq.skip (Seq.length groups + 5) |> Seq.map parseTicket
    groups, ticket, otherTickets

parse lines |> fun (a, b, c) -> a

let inRangeGroup x group =
    snd group |> Seq.exists (fun (min, max) -> min <= x && x <= max)

let inRange groups x =
    groups |> Seq.exists (inRangeGroup x)

let mainSimple lines =
    let groups, ticket, otherTickets = parse lines
    let not f x = not (f x)
    otherTickets 
    |> Seq.collect (Seq.filter (not (inRange groups)))
    |> Seq.sum

mainSimple lines

inputLines |> mainSimple |> printfn "%A"

let mainComplex lines =
    let groups, ticket, otherTickets = parse lines
    let otherTickets = otherTickets |> Seq.filter (Seq.forall (inRange groups))
    let allTickets = Seq.append (Seq.singleton ticket) otherTickets
    let possibleDomains = 
        Seq.init (Seq.length groups) (fun i -> 
            groups 
            |> Seq.filter (fun group ->
                    allTickets |> Seq.forall (fun otherTicket ->
                        let item = Seq.item i otherTicket
                        inRangeGroup item group
                    ))
            |> Seq.map fst
            |> Set
        )
    printfn "%A" possibleDomains

    let rec findSolution possibleDomains groupNamesToRemove removedNames  =
        printfn "%A\n%A\n%A\n======\n\n" possibleDomains groupNamesToRemove removedNames
        let newPossibleDomains = possibleDomains |> Seq.map (fun possibleGroups -> 
            if Set.count possibleGroups = 1 then possibleGroups else Set.difference possibleGroups groupNamesToRemove)
        if Seq.forall (fun possibleSet -> Set.count possibleSet <= 1) newPossibleDomains then
            newPossibleDomains
        else
            let foundGroups = newPossibleDomains |> Seq.filter (fun x -> Set.count x = 1) |> Seq.collect id |> Set
            let newFoundGroups = Set.difference foundGroups removedNames
            if Set.count newFoundGroups = 0 then
                printfn "%A" possibleDomains
            findSolution newPossibleDomains newFoundGroups (Set.union groupNamesToRemove removedNames)
    let assingedGroups = findSolution possibleDomains Set.empty Set.empty
    printfn "%A" assingedGroups

    assingedGroups
    |> Seq.map (fun set -> Seq.head set)
    |> Seq.indexed
    |> Seq.filter (fun (_, (name: string)) -> name.StartsWith("departure"))
    |> Seq.map fst
    |> Seq.map (fun i -> Seq.item i ticket |> int64)
    |> Seq.fold (fun a b -> a * b) 1L

mainComplex lines

mainComplex inputLines |> printfn "%A"

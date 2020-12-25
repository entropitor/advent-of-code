#load "shared.fsx"

open System
open Shared

let lines =
    let input = """light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."""
    input.Split("\n")

open System.Text.RegularExpressions

let parse lines =
    let parseContent content =
        let m = Regex.Match(content, "(\d+) (.*) bags?")

        System.Int32.Parse(m.Groups.[1].Value), m.Groups.[2].Value

    let parseContents (contents: String) =
        if contents = "no other bags."
        then Seq.empty
        else contents.Split(", ") |> Seq.map parseContent

    let parseLine line =
        let m =
            Regex.Match(line, "(.*?) bags contain (.*)")

        m.Groups.[1].Value, (parseContents m.Groups.[2].Value)

    let tree = lines |> Seq.map parseLine
    tree |> Map

let typesCanContainShinyGold lines =
    let tree = parse lines

    let rec contains accumulator toBeFound =
        if Set.count toBeFound = 0 then
            accumulator
        else
            let extraOptions =
                toBeFound
                (* |> Seq.collect (fun x -> Map.find x tree |> Seq.map snd) *)
                |> Seq.collect (fun toBeFound ->
                    tree
                    |> Seq.filter (fun keyValue ->
                        keyValue.Value
                        |> Seq.map snd
                        |> Seq.contains toBeFound)
                    |> Seq.map (fun keyValue -> keyValue.Key))
                |> Set

            contains (Set.union accumulator extraOptions) (Set.difference extraOptions accumulator)

    contains Set.empty (Set.singleton "shiny gold")
    |> Set.count

let bagsInShinyGold lines =
    let tree = parse lines

    let keysOf map = map |> Map.toSeq |> Seq.map fst |> Set

    let rec count knownSizes =
        let unknownKeys =
            Set.difference (keysOf tree) (keysOf knownSizes)

        let knownKeys =
            unknownKeys
            |> Seq.filter (fun key ->
                let value = Map.find key tree

                value
                |> Seq.map snd
                |> Seq.forall (fun key -> Map.containsKey key knownSizes))

        if Seq.length knownKeys = 0 then
            knownSizes
        else
            let newKnownSizes =
                knownKeys
                |> Seq.map (fun key ->
                    let value = Map.find key tree

                    let score =
                        value
                        |> Seq.map (fun (count, key) -> count * Map.find key knownSizes)
                        |> Seq.sum

                    key, score + 1)

            count
                (Map.toSeq knownSizes
                 |> Seq.append newKnownSizes
                 |> Map)

    (count Map.empty |> Map.find "shiny gold") - 1

(* A *)
assertEqual 4 (typesCanContainShinyGold lines)

typesCanContainShinyGold inputLines
|> printfn "%A"

(* B *)
assertEqual 32 (bagsInShinyGold lines)

bagsInShinyGold inputLines |> printfn "%A"

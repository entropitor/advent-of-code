#load "shared.fsx"

open Shared
open System

let testInput =
    seq {
        "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
        "byr:1937 iyr:2017 cid:147 hgt:183cm"
        ""
        "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
        "hcl:#cfa07d byr:1929"
        ""
        "hcl:#ae17e1 iyr:2013"
        "eyr:2024"
        "ecl:brn pid:760753108 byr:1931"
        "hgt:179cm"
        ""
        "hcl:#cfa07d eyr:2025 pid:166559648"
        "iyr:2011 ecl:brn hgt:59in"
    }

let rec parse lines =
    let isEmpty line = line = ""

    let parseKeyValue (str: String) =
        let parts = str.Split(":")
        parts.[0], parts.[1]

    let parseOne (lines: seq<String>) =
        seq {
            for line in lines do
                yield! line.Split(" ") |> Seq.map parseKeyValue
        }
        |> Map

    let one = Seq.takeWhile (isEmpty >> not) lines

    if Seq.length one = 0 then
        Seq.empty
    else
        seq {
            yield parseOne one

            yield!
                parse
                    (lines
                     |> Seq.skip (Seq.length one)
                     |> Seq.skipWhile isEmpty)
        }
(*
parse testInput
*)

let isValidSimple passport =
    if passport |> Map.containsKey "cid" then Map.count passport >= 8 else Map.count passport >= 7

open System.Text.RegularExpressions

let (|FirstRegexGroup|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if (m.Success) then Some m.Groups.[1].Value else None

let isValidComplex passport =
    let tryParse (x: String) =
        let success, value = Int32.TryParse x
        if success then Some value else None

    let fourDigitsBetween low high x =
        match x with
        | FirstRegexGroup "(\d{4})" value ->
            tryParse value
            |> Option.bind (fun value -> Some(low <= value && value <= high))
        | _ -> None

    let validateHeight x =
        match x with
        | FirstRegexGroup "(\d*)cm" height ->
            tryParse height
            |> Option.bind (fun height -> Some(150 <= height && height <= 193))
        | FirstRegexGroup "(\d*)in" height ->
            tryParse height
            |> Option.bind (fun height -> Some(59 <= height && height <= 76))
        | _ -> None

    let validateHairColor x =
        match x with
        | FirstRegexGroup "#([0-9a-fA-F]{6})" _ -> Some true
        | _ -> None

    let validatePassportNumber x =
        match x with
        | FirstRegexGroup "(\d{9})" _ -> Some true
        | _ -> None

    let validateEyeColor x =
        seq {
            "amb"
            "blu"
            "brn"
            "gry"
            "grn"
            "hzl"
            "oth"
        }
        |> Seq.tryFind (fun y -> x = y)
        |> Option.map (fun _ -> true)

    seq {
        passport
        |> Map.tryFind "byr"
        |> Option.bind (fourDigitsBetween 1920 2002)

        passport
        |> Map.tryFind "iyr"
        |> Option.bind (fourDigitsBetween 2010 2020)

        passport
        |> Map.tryFind "eyr"
        |> Option.bind (fourDigitsBetween 2020 2030)

        passport
        |> Map.tryFind "hgt"
        |> Option.bind validateHeight

        passport
        |> Map.tryFind "hcl"
        |> Option.bind validateHairColor

        passport
        |> Map.tryFind "ecl"
        |> Option.bind validateEyeColor

        passport
        |> Map.tryFind "pid"
        |> Option.bind validatePassportNumber
    }
    |> Seq.forall (fun x -> x = Some true)

let main isValid lines =
    lines |> parse |> Seq.filter isValid |> Seq.length

(* A *)
assertEqual (main isValidSimple testInput) 2

inputLines |> main isValidSimple |> printfn "%A"

(* B *)
assertEqual (main isValidComplex testInput) 2

inputLines |> main isValidComplex |> printfn "%A"

let validComplexPassports =
    seq {
        "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
        "hcl:#623a2f"
        ""
        "eyr:2029 ecl:blu cid:129 byr:1989"
        "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
        ""
        "hcl:#888785"
        "hgt:164cm byr:2001 iyr:2015 cid:88"
        "pid:545766238 ecl:hzl"
        "eyr:2022"
        ""
        "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
    }

validComplexPassports
|> main isValidComplex
|> assertEqual 4

validComplexPassports
|> parse
|> Seq.head
|> isValidComplex

let invalidComplexPassports =
    seq {
        "eyr:1972 cid:100"
        "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
        ""
        "iyr:2019"
        "hcl:#602927 eyr:1967 hgt:170cm"
        "ecl:grn pid:012533040 byr:1946"
        ""
        "hcl:dab227 iyr:2012"
        "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
        ""
        "hgt:59cm ecl:zzz"
        "eyr:2038 hcl:74454a iyr:2023"
        "pid:3556412378 byr:2007"
    }

invalidComplexPassports
|> main isValidComplex
|> assertEqual 0

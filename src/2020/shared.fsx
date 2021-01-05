open System

let inputLines =
    let lines =
        let read _ = Console.ReadLine()
        let isValid x = x <> null
        Seq.initInfinite read |> Seq.takeWhile isValid

    Seq.cache lines

let assertEqual value expected =
    if value = expected
    then ()
    else failwith (sprintf "Expected %A to equal %A" value expected)

let split (str: string) = str.Split("\n")

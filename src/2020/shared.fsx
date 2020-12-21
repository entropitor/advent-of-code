open System

let inputLines =
  let read _ = Console.ReadLine()
  let isValid x = x <> null
  Seq.initInfinite read |> Seq.takeWhile isValid

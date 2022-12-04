let lines = Seq.of_dispenser (fun _ -> In_channel.input_line stdin)

let elves =
  lines
  |> Seq.map (fun i ->
         match i with
         | "" -> None
         | _ -> Some (Scanf.sscanf i "%d" (fun x -> x)))
  |> Seq.fold_left
       (fun (elf, elves) lineItem ->
         match lineItem with
         | None -> ([], elf :: elves)
         | Some item -> (item :: elf, elves))
       ([], [])
  |> fun (elf, elves) -> elf :: elves

let totalPerElves =
  elves
  |> List.map (fun items ->
         items |> List.fold_left (fun acc item -> acc + item) 0)

let part1 = totalPerElves |> List.fold_left (fun acc total -> max acc total) 0

let part2 =
  totalPerElves
  |> List.sort (fun x y -> ~-(Int.compare x y))
  |> List.to_seq |> Seq.take 3
  |> Seq.fold_left (fun acc x -> acc + x) 0
;;

print_int part1;;
print_newline;;
print_int part2

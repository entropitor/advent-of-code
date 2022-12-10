let lines =
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb\n\
   bvwbjplbgvbhsrlpgdmjqwftvncz\n\
   nppdvjthqldpwncqszvftbrmjlhg\n\
   nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg\n\
   zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" |> String.split_on_char '\n' |> List.to_seq

let input_lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

module CharSet = Set.Make (Char)

let first_line lines = lines |> List.of_seq |> List.hd

let solve nb line =
  let chars = line |> String.to_seq in
  chars
  |> Seq.fold_lefti
       (fun acc i chr ->
         match acc with
         | lst, Some j -> ([], Some j)
         | lst, None ->
             let new_list =
               chr :: lst |> List.to_seq |> Seq.take nb |> List.of_seq
             in
             let set_size =
               CharSet.of_list new_list |> CharSet.to_seq |> Seq.length
             in
             if set_size == nb then ([], Some (i + 1)) else (new_list, None))
       ([], None)
  |> snd
;;

lines |> Seq.map (solve 4) |> List.of_seq;;
Printf.printf "Part 1: %d\n" (input_lines |> first_line |> solve 4 |> Option.get)
;;
lines |> Seq.map (solve 14) |> List.of_seq;;

Printf.printf "Part 2: %d\n"
  (input_lines |> first_line |> solve 14 |> Option.get)

let lines =
  "$ cd /\n\
   $ ls\n\
   dir a\n\
   14848514 b.txt\n\
   8504156 c.dat\n\
   dir d\n\
   $ cd a\n\
   $ ls\n\
   dir e\n\
   29116 f\n\
   2557 g\n\
   62596 h.lst\n\
   $ cd e\n\
   $ ls\n\
   584 i\n\
   $ cd ..\n\
   $ cd ..\n\
   $ cd d\n\
   $ ls\n\
   4060174 j\n\
   8033020 d.log\n\
   5626152 d.ext\n\
   7214296 k" |> String.split_on_char '\n' |> List.to_seq

let input_lines =
  Seq.of_dispenser (fun _ -> In_channel.input_line stdin) |> Seq.memoize

type entry = Dir of string | File of int * string
type command = Cd of string | Ls of entry list

type tree =
  | Directory of (string, tree) Hashtbl.t
  | FileEntry of int
  | UnknownDirectory

let rec add_to_tree (path : string list) entries tree =
  match path with
  | [] -> (
      match tree with
      | Directory subDirectories -> failwith "already ls'ed in here"
      | FileEntry _ -> failwith "in a file?"
      | UnknownDirectory ->
          let tbl = Hashtbl.create 100 in
          let _ =
            entries
            |> List.map (fun entry ->
                   match entry with
                   | File (size, name) -> Hashtbl.add tbl name (FileEntry size)
                   | Dir name -> Hashtbl.add tbl name UnknownDirectory)
          in
          Directory tbl)
  | hd :: tl -> (
      match tree with
      | Directory subDirectories ->
          let _ =
            Hashtbl.replace subDirectories hd
              (add_to_tree tl entries (Hashtbl.find subDirectories hd))
          in
          Directory subDirectories
      | FileEntry _ -> failwith "cd'ing into a file"
      | UnknownDirectory -> failwith "cd'ing into unknown directory")

let parse lines =
  lines
  |> Seq.group (fun line_a line_b ->
         not (String.starts_with ~prefix:"$ " line_b))
  |> Seq.map (fun lines ->
         let lines = List.of_seq lines in
         let cmd = List.hd lines in
         let response = List.tl lines in
         let _ = Printf.printf "Command: '%s'\n" cmd in
         if cmd = "$ ls" then
           Ls
             (response
             |> List.map (fun entry ->
                    if String.starts_with ~prefix:"dir " entry then
                      Scanf.sscanf entry "dir %s" (fun d -> Dir d)
                    else
                      Scanf.sscanf entry "%d %s" (fun size name ->
                          File (size, name))))
         else Scanf.sscanf cmd "$ cd %s" (fun dir -> Cd dir))
  |> Seq.fold_left
       (fun (cwd, tree) command ->
         match command with
         | Cd "/" -> ([], tree)
         | Cd ".." -> (List.tl cwd, tree)
         | Cd dir -> (dir :: cwd, tree)
         | Ls files ->
             let new_tree = add_to_tree (List.rev cwd) files tree in
             (cwd, new_tree))
       ([], UnknownDirectory)

let rec solve1 acc tree =
  match tree with
  | UnknownDirectory -> failwith "UnknownDirectory"
  | FileEntry size -> (size, 0)
  | Directory tbl ->
      let size_children, sol_children =
        Hashtbl.to_seq tbl
        |> Seq.map (fun (name, tree) ->
               let _ = Printf.printf "Counting %s\n" name in
               solve1 0 tree)
        |> Seq.fold_left
             (fun (size_acc, sol_acc) (size_next, sol_next) ->
               (size_acc + size_next, sol_acc + sol_next))
             (0, 0)
      in
      let _ = Printf.printf "%d %d\n" sol_children size_children in
      if size_children < 100000 then
        (size_children, sol_children + size_children)
      else (size_children, sol_children)
;;

parse lines |> snd |> solve1 0 |> snd;;
Printf.printf "Part 1: %d\n" (parse input_lines |> snd |> solve1 0 |> snd)

let rec solve2 tree =
  let rec get_size tree =
    match tree with
    | UnknownDirectory -> failwith "UnknownDirectory"
    | FileEntry size -> size
    | Directory tbl ->
        Hashtbl.to_seq_values tbl |> Seq.map get_size |> Seq.fold_left ( + ) 0
  in
  let used_size = get_size tree in
  let free_size = 70000000 - used_size in
  let min_size_to_delete = 30000000 - free_size in
  let rec get_smallest_directory_to_delete acc tree =
    match tree with
    | UnknownDirectory -> failwith "UnknownDirectory"
    | FileEntry size -> None
    | Directory tbl -> (
        let sol_children =
          Hashtbl.to_seq_values tbl
          |> Seq.map (fun tree -> get_smallest_directory_to_delete None tree)
          |> Seq.fold_left
               (fun acc next ->
                 match (acc, next) with
                 | Some a, Some b -> Some (min a b)
                 | Some a, None -> Some a
                 | None, Some b -> Some b
                 | None, None -> None)
               None
        in
        match sol_children with
        | None ->
            let size = get_size tree in
            if size >= min_size_to_delete then Some size else None
        | Some a -> Some a)
  in
  get_smallest_directory_to_delete None tree
;;

parse lines |> snd |> solve2;;
Printf.printf "Part 2: %d\n" (parse input_lines |> snd |> solve2 |> Option.get)

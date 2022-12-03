let parse lines =
  fst
  @@ List.fold_left
       (fun (acc, elf) line ->
         if line = "" then
           match elf with [] -> (acc, []) | _ -> (elf :: acc, [])
         else (acc, int_of_string line :: elf))
       ([], []) lines

let read_all f =
  let inc = In_channel.open_text f in
  let a = In_channel.input_all inc in
  In_channel.close inc;
  a

let top_3 l =
  match List.sort (fun a b -> Int.compare b a) l with
  | a :: b :: c :: _ -> [ a; b; c ]
  | _ -> failwith "not 3"

let sum_l = List.fold_left ( + ) 0

let run f =
  let contents = String.split_on_char '\n' (read_all f) in
  let elfs = parse contents in
  let sum = List.map sum_l elfs in
  Fmt.pr "%d\n" (sum_l (top_3 sum))

open Containers
module CharSet = Set.Make (Char)

let split s =
  let mid = String.length s / 2 in
  let s1, s2 =
    String.to_seqi s
    |> Seq.fold_left
         (fun (s1, s2) (idx, char) ->
           if idx < mid then (CharSet.add char s1, s2)
           else (s1, CharSet.add char s2))
         (CharSet.empty, CharSet.empty)
  in
  (s1, s2)

let score = function
  | 'a' .. 'z' as char -> Char.code char - Char.code 'a' + 1
  | 'A' .. 'Z' as char -> Char.code char - Char.code 'A' + 27
  | _ -> failwith "error"

let find_common s1 s2 =
  let s3 = CharSet.inter s1 s2 in
  CharSet.choose_opt s3

let find_commons sets =
  match sets with
  | [] -> CharSet.empty
  | hd :: tl -> List.fold_left CharSet.inter hd tl

let file = "input"

let main seq =
  seq
  |> Seq.filter_map (fun l ->
         let s1, s2 = split l in
         find_common s1 s2)
  |> Utils.sum_f score

let part_2 seq =
  seq |> Utils.window 3
  |> Seq.filter_map (fun l ->
         List.map
           (fun str ->
             String.fold (fun acc c -> CharSet.add c acc) CharSet.empty str)
           l
         |> find_commons |> CharSet.choose_opt)
  |> Utils.sum_f score

let run file =
  let seq = Utils.lines file in
  main seq |> Fmt.pr "%d\n";
  part_2 seq |> Fmt.pr "%d\n"

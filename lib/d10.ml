type inst = Add of int | Noop [@@deriving show { with_path = false }]

module Parse = struct
  open Angstrom

  let noop = string "noop" >>| fun _ -> Noop

  let add =
    let open Utils.Parse in
    let* _ = string "addx" <* whitespace in
    let+ n = int in
    Add n

  let parse = noop <|> add

  let parse string =
    parse_string ~consume:Consume.All parse string |> Result.get_ok
end

let execute seq f =
  let rec aux seq x cycle =
    f cycle x;
    match seq () with
    | Seq.Nil -> ()
    | Seq.Cons (Noop, seq) -> aux seq x (cycle + 1)
    | Seq.Cons (Add v, seq) ->
        f (cycle + 1) x;
        aux seq (x + v) (cycle + 2)
  in
  aux seq 1 1

let part1 seq =
  let v = ref 0 in
  execute seq (fun cycle x ->
      match cycle with
      | 20 | 60 | 100 | 140 | 180 | 220 -> v := !v + (cycle * x)
      | _ -> ());
  !v

let print_display arr =
  for i = 0 to 239 do
    if i mod 40 = 0 then print_newline ();
    if arr.(i) = 0 then print_char ' ' else print_string "\u{2588}"
  done

let part2 seq =
  let array = Array.init 240 (fun _ -> 0) in
  execute seq (fun cycle x ->
      let pos = cycle - 1 in
      if cycle > 240 then ()
      else
        let column = pos mod 40 in
        if abs (column - x) <= 1 then array.(pos) <- 1 else ());
  array

let run f =
  let seq = Utils.lines f |> Seq.map Parse.parse in
  part1 seq |> Printf.printf "%d\n";
  part2 seq |> print_display;
  print_newline ()

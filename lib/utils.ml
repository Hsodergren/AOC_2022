let lines ?(skip_empty=true) f =
  let inc = In_channel.open_text f in
  let rec f () =
    match In_channel.input_line inc with
    | None ->
        In_channel.close inc;
        Seq.Nil
    | Some "" -> if skip_empty then f () else Seq.Cons ("", f)
    | Some l -> Seq.Cons (l, f)
  in
  f |> Seq.memoize

let chars f =
  let inc = In_channel.open_text f in
  let rec f () =
    match In_channel.input_char inc with
    | None ->
       In_channel.close inc;
       Seq.Nil
    | Some char -> Seq.Cons (char, f)
  in
  f |> Seq.memoize

let read_all f =
  let inc = open_in f in
  let res = In_channel.input_all inc in
  In_channel.close inc;
  res

(* [repeat_n n v] returns a Seq.t with the value v repeated n times *)
let rec repeat_n n v () =
  if n = 0 then Seq.Nil
  else Seq.Cons (v, repeat_n (n-1) v)

let sliding_window n seq =
  let rec rem_last l =
    match l with
    | [_] | [] -> []
    | hd::tl -> hd::rem_last tl
  in
  let rec aux seq acc () =
    match seq () with
    | Seq.Nil -> Seq.Cons(List.rev acc, fun () -> Seq.Nil)
    | Seq.Cons (v,seq) ->
       if List.length acc == n
       then
         let new_acc = v::(rem_last acc) in
         Seq.Cons (List.rev acc, aux seq new_acc)
       else aux seq (v::acc) ()
  in
  aux seq []

let window n seq =
  let rec aux values_left seq acc () =
    if values_left = 0 then Seq.Cons (acc, aux n seq [])
    else
      match seq () with
      | Seq.Cons (v, s) -> aux (values_left - 1) s (v :: acc) ()
      | Seq.Nil -> Seq.Nil
  in
  aux n seq []

let lines_l f = lines f |> List.of_seq
let sum = Seq.fold_left ( + ) 0
let sum_f f = Seq.fold_left (fun acc v -> acc + f v) 0
let count f seq = Seq.map (fun v -> if f v then 1 else 0) seq |> sum

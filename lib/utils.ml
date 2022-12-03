let lines f =
  let inc = In_channel.open_text f in
  let rec f () =
    match In_channel.input_line inc with
    | None ->
        In_channel.close inc;
        Seq.Nil
    | Some "" -> f ()
    | Some l -> Seq.Cons (l, f)
  in
  f |> Seq.memoize

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

module CharSet = Set.Make(Char)
let all_different l =
  let exception Same in
  try
    ignore @@ List.fold_left (fun acc v ->
                  if CharSet.mem v acc then raise Same
                  else CharSet.add v acc
                ) CharSet.empty l;
    true
  with Same -> false


let doit seq n =
  let exception Found of int in
  try
    seq |> Utils.sliding_window n
    |> Seq.iteri (fun i l -> if all_different l then raise (Found (i+n)) )
  with Found i -> Printf.printf "%d\n" i

let run f =
    let seq = Utils.chars f in
    doit seq 4;
    doit seq 14

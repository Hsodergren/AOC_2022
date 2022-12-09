module Move = struct
  type t = Up
         | Down
         | Right
         | Left [@@deriving show]
end

module Pos = struct
  type t = int * int [@@deriving ord, show]
  let zero = 0,0

  let move move (x,y) =
    match move with
    | Move.Up -> x,y+1
    | Move.Down -> x,y-1
    | Move.Right -> x+1,y
    | Move.Left -> x-1,y

  let sign i = if i < 0 then -1 else if i > 0 then 1 else 0

  let follow ~master:(x1,y1) ~follower:(x2,y2) =
    let dx, dy = x1-x2, y1-y2 in
    if dx >= -1 && dx <= 1 && dy >= -1 && dy <= 1 then (x2,y2)
    else x2+sign dx,y2+sign dy
end

let parse seq =
  let p =
    let open Angstrom in
    let int = take_while1 (function | '0'..'9' -> true | _ -> false) >>| int_of_string in
    let* p = any_char <* advance 1 >>| function
             | 'R' -> Move.Right
             | 'U' -> Move.Up
             | 'L' -> Move.Left
             | 'D' -> Move.Down
             | _ -> failwith "error"
    in
    let+ num = int in
    Utils.repeat_n num p
  in
  Seq.concat_map (fun str -> Angstrom.parse_string ~consume:Angstrom.Consume.All p str |> Result.get_ok) seq

module PosSet = Set.Make(Pos)

let simulate seq len =
  let step l dir =
    let rec aux l =
      match l with
      | hd1::hd2::tl ->
         let last,tail = aux ((Pos.follow ~master:hd1 ~follower:hd2)::tl) in
         last, hd1::tail
      | [last] -> last,l
      | _ -> failwith "error"
    in
    match l with
    | hd::tl -> aux (Pos.move dir hd::tl)
    | [] -> failwith "error"
  in
  Seq.fold_left (fun (set,state) move ->
      let last, state = step state move in
      PosSet.add last set, state
    ) (PosSet.empty, List.init len (fun _ -> Pos.zero)) seq
  |> fst
  |> PosSet.cardinal

let run f =
  let seq = Utils.lines f |> parse in
  simulate seq 2  |> Printf.printf "%d\n";
  simulate seq 10 |> Printf.printf "%d\n";

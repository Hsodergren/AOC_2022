module Forest = struct
  module Pos = struct
    type t = int * int [@@deriving ord,show]
    let zero = 0,0
    let up (y,x) = y-1,x
    let down (y,x) = y+1,x
    let right (y,x) = y,x+1
    let left (y,x) = y,x-1

    let distance (y1,x1) (y2,x2) = abs (y1-y2+x1-x2)
  end
  type t = int array array * int (* height *) * int (* width *)

  let valid (y,x) (_,h,w) = not (y < 0 || y >= h || x < 0 || x >= w)
  let value (y,x) (t,_,_) = t.(y).(x)

  let next (y,x) (_,h,w) =
    if x+1 = w then if y+1 = h
                    then None
                    else Some (y+1,0)
    else Some (y,x+1)

  let fold f init (t:t) =
    let rec aux acc pos =
      match f acc (value pos t) pos with
      | `Stop acc -> acc
      | `Cont acc ->
         match next pos t with
         | None -> acc
         | Some pos -> aux acc pos
    in
    aux init Pos.zero

  let rec find f next_pos_f start (t:t) =
    let next = next_pos_f start in
    if not (valid next t) then start
    else if f next then next
    else find f next_pos_f next t

  let rec for_all f next_pos_f start (t:t) =
    if valid start t
    then f start && for_all f next_pos_f (next_pos_f start) t
    else true

end

let parse f =
  let arr = Utils.lines f |> Seq.map (fun s -> String.to_seq s |> Seq.map (fun c -> Char.code c - Char.code '0') |> Array.of_seq) |> Array.of_seq in
  let height = Array.length arr in
  let width = Array.length arr.(0) in
  arr,height,width

let part1 forest =
 Forest.fold (fun acc v pos ->
      let find pos_f =
        Forest.for_all (fun s_pos -> if Forest.Pos.compare s_pos pos = 0 then true else Forest.value s_pos forest < v) pos_f pos forest
      in
      let visible = Forest.Pos.(find up || find down || find right || find left) in
      if visible
      then `Cont (v::acc)
      else `Cont acc
    ) [] forest

let part2 forest =
  Forest.fold (fun acc v pos ->
      let view_distance dir =
        let new_pos = Forest.find (fun s_pos ->
            let s_height = Forest.value s_pos forest in
            s_height >= v
          ) dir pos forest
        in
        Forest.Pos.distance pos new_pos
      in
      let score = Forest.Pos.(view_distance up * view_distance right * view_distance down * view_distance left) in
      if score > acc then `Cont score else `Cont acc
      ) 0 forest

let run f =
  let forest = parse f in
  part1 forest |> (fun l -> Fmt.pr "%d\n" (List.length l));
  part2 forest |> Fmt.pr "%d\n"

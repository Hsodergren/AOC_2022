type move = | Rock
            | Paper
            | Scissor


type res = | Win | Lose | Draw

(* let move_pp ppf = function *)
(*   | Rock -> Fmt.pf ppf "Rock" *)
(*   | Paper -> Fmt.pf ppf "Paper" *)
(*   | Scissor -> Fmt.pf ppf "Scissor" *)
(* let res_pp ppf = function *)
(*   | Win -> Fmt.pf ppf "Win" *)
(*   | Lose -> Fmt.pf ppf "Lose" *)
(*   | Draw -> Fmt.pf ppf "Draw" *)

let game p1 p2 =
  match p1,p2 with
  | Rock, Rock
    | Paper,Paper
    | Scissor,Scissor -> Draw
  | Rock, Scissor
    | Paper, Rock
    | Scissor, Paper -> Win
  | _ -> Lose

let score res move =
  let score_res = function
    | Win -> 6
    | Draw -> 3
    | Lose -> 0
  in
  let score_move = function
    | Rock -> 1
    | Paper -> 2
    | Scissor -> 3
  in
  score_res res + score_move move


let parse_abc = function
  | 'A' -> Rock
  | 'B' -> Paper
  | 'C' -> Scissor
  | _ -> failwith "error1"

let parse_1 = function
  | 'X' -> Rock
  | 'Y' -> Paper
  | 'Z' -> Scissor
  | _ -> failwith "error2"

let parse_2 = function
  | 'X' -> Lose
  | 'Y' -> Draw
  | 'Z' -> Win
  | _ -> failwith "error2"

let lines f =
  let inc = In_channel.open_text f in
  let rec f () =
    match In_channel.input_line inc with
    | None -> In_channel.close inc; Seq.Nil
    | Some "" -> f ()
    | Some l -> Seq.Cons (l, f)
  in
  f

let parse_part2 l =
  let a = String.get l 0 in
  let b = String.get l 2 in
  parse_abc a, parse_2 b

let parse_part1 l =
  let a = String.get l 0 in
  let b = String.get l 2 in
  parse_abc a, parse_1 b

let run lines parsef p1_fun =
  lines |> Seq.map parsef
  |> Seq.map (fun (p2,p1) ->
         let p1 = p1_fun p2 p1 in
         let result = game p1 p2 in
         score result p1
       )
  |> Seq.fold_left (+) 0

let part1 seq =
  let sum = run seq parse_part1 (fun _ a -> a) in
  Fmt.pr "%d\n" sum

let part2 seq =
  let sum = run seq parse_part2
              (fun a b ->
                match b with
                | Draw -> a
                | Win -> begin
                    match a with
                    | Paper -> Scissor
                    | Rock -> Paper
                    | Scissor -> Rock
                  end
                | Lose -> begin
                    match a with
                    | Paper -> Rock
                    | Rock -> Scissor
                    | Scissor -> Paper
                  end
              )
  in
  Fmt.pr "%d\n" sum

let f = "input"
let () = part1 (lines f)
let () = part2 (lines f)

open Containers

module IMap = Map.Make(Int)

let split_on f seq =
  let rec aux seq acc =
    match seq () with
    | Seq.Nil -> acc, seq
    | Seq.Cons (v,s) ->
       if f v
       then (acc, s)
       else aux s (v::acc)
  in
  aux seq []

let int_p = Angstrom.(take_while (function '0'..'9' -> true | _ -> false) >>| int_of_string)
let parse lines =
  let parser_l =
    let open Angstrom in
    let field_parser = char '[' *> any_char <* char ']' >>| Option.some in
    let empty_parser =  string "   " >>| fun _ -> None in
    let par = field_parser <|> empty_parser in
    sep_by1 (char ' ') par
  in
  match lines with
  | [] -> failwith "parse empty list"
  | _::tl ->
     List.fold_left
       (fun acc str ->
         match Angstrom.parse_string ~consume:Angstrom.Consume.All parser_l str with
         | Ok res ->
            (List.fold_left (fun (acc,i) -> function
                | Some c -> IMap.update i (function | Some l -> Some (c::l) | None -> Some [c]) acc, i+1
                | None -> acc,i+1
               ) (acc,1) res) |> fst
         | Error str -> failwith str
       ) IMap.empty tl

let move_parser =
  let open Angstrom in
  let* n = string "move " *> int_p in
  let* f = string " from " *> int_p in
  let+ t = string " to " *> int_p in
  (n,f,t)

let remove i state =
  match IMap.get i state with
  | Some (hd::tl) ->
     let ret = IMap.add i tl state in
     hd,ret
  | Some _ -> failwith "stack empty"
  | None -> failwith "unknown key"

let add i v state =
  IMap.update i (function | Some l -> Some (v::l) | None -> failwith "add unknown key") state

let move from' to' state =
  let v, state = remove from' state in
  add to' v state

let rec move_n n from' to' state =
  if n = 0 then state else
  move_n (n-1) from' to' (move from' to' state)

let remove_n n i state =
  match IMap.get i state with
  | Some l ->
     let hd,tl = List.take_drop n l in
     let ret = IMap.add i tl state in
     hd,ret
  | None -> failwith "unknown key"

let add_n i v state =
  IMap.update i (function | Some l -> Some (v@l) | None -> failwith "add unknown key") state

let move_n_2 n from' to' state =
  let move,state = remove_n n from' state in
  add_n to' move state

let print_top state =
  IMap.iter (fun _ -> function | hd::_ -> print_char hd | [] -> failwith "empty stack") state;
  print_newline ()

let do_it state seq movef =
  Seq.fold_left (fun state str ->
      match Angstrom.parse_string ~consume:Angstrom.Consume.All move_parser str with
      | Ok (n,f,t) -> movef n f t state
      | Error msg -> failwith msg
    ) state seq

let run f =
  let lines = Utils.lines ~skip_empty:false f in
  let header, cont = split_on (String.equal "") lines in
  let start_state = parse header in
  do_it start_state cont move_n |> print_top;
  do_it start_state cont move_n_2 |> print_top

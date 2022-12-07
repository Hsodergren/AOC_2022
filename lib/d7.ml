open Containers
module Parser = struct
  open Angstrom
  type ls_data = Dir of string
               | File of int * string [@@deriving show]
  type cmd = Cd of [`Back | `Into of string | `Root]
           | Ls of ls_data list [@@deriving show]

  type cmds = cmd list [@@deriving show]

  let word = take_while1 (function ' ' | '\n' | '\t' | '$' -> false | _ -> true)
  let int = take_while1 (function '0'..'9' -> true | _ -> false) >>| int_of_string
  let newline = char '\n' >>| fun _ -> ()

  let cd = string "cd " *> word >>| function ".." -> Cd `Back
                                           | "/" -> Cd `Root
                                           | s -> Cd (`Into s)

  let file =
    let* size = int <* char ' ' in
    let+ name = word in
    File (size,name)

  let dir =
    let+ name = string "dir " *> word in
    Dir name

  let ls_data = file <|> dir
  let ls =
    let* _ = Angstrom.peek_string 2 in
    let* () = string "ls" *> newline in
    sep_by newline ls_data >>| fun data -> Ls data

  let cmd =
    let* _ = string "$ " in
    cd <|> ls

  let cmds =
    sep_by newline cmd
end

module SMap = Map.Make(String)

type file = Dir of file SMap.t
          | File of int

let rec pp ppf = function
  | File int -> Fmt.pf ppf "File %d@ " int
  | Dir map -> Fmt.pf ppf "Dir (@ %a@ )@ " Fmt.Dump.(list (pair string pp)) (SMap.bindings map)

let t_of_lsdata =
  List.fold_left (fun acc -> function
      | Parser.File (s,n) -> SMap.add n (File s) acc
      | Dir n -> SMap.add n (Dir SMap.empty) acc
    ) SMap.empty

type tsmap = file SMap.t
let pp_tsmap = SMap.pp String.pp pp

type state = {
    path: string list;
    root: tsmap
  } [@@deriving show {with_path=false}]
let empty_state = {path=[]; root=SMap.empty}

let updatef f = function
  | Some v -> Some (f v)
  | None -> None

let parse_t cmds =
  let rec aux cmds state =
    let rec update_state root path files : file SMap.t =
      match path with
      | hd::tl -> SMap.update hd (updatef (fun v ->
                                      match v with
                                      | File _ -> failwith "cannot go into a file"
                                      | Dir t -> Dir (update_state t tl files))) root
      | [] -> t_of_lsdata files
    in
    (* Fmt.pr "%a@ " pp_state state; *)
    match cmds with
    | Parser.Cd `Back::tl ->aux tl {state with path=List.tl state.path}
    | Cd `Into s::tl -> aux tl {state with path=s::state.path}
    | Cd `Root::tl -> aux tl {state with path=[]}
    | Ls files::tl -> (* Fmt.pr "%a\n" (SMap.pp String.pp pp) state.root; *) aux tl {state with root=update_state state.root (List.rev state.path) files}
    | [] -> state
  in
  (aux cmds empty_state).root

let rec fs_size fs =
  SMap.fold (fun _ t size ->
      match t with
      | File s -> size+s
      | Dir t ->
         size + fs_size t
    ) fs 0

let part1 fs =
  let counter = ref 0 in
  let rec aux fs : int=
    let size =
      SMap.fold (fun _ t size ->
        match t with
        | File s -> size+s
        | Dir t ->
           size + aux t
      ) fs 0
    in
    if size <= 100_000 then (
      Fmt.pr "%d\n" size;
      counter := !counter + size;
      size
    ) else
      size
  in
  ignore @@ aux fs;
  !counter

let part2 fs update_size =
  let tot_size = fs_size fs in
  let unused = 70_000_000 - tot_size in
  let need_to_remove = update_size - unused in
  Fmt.pr "%d %d %d\n" tot_size unused need_to_remove;
  let rec aux fs (best,size) =
    SMap.fold (fun _ t (best,orig_size) ->
        match t with
        | File _ -> best,orig_size
        | Dir t ->
           let size = fs_size t in
           let a = size - need_to_remove in
           let best_so_far = if a > 0 && a < best then a,size else best,orig_size in
           aux t best_so_far
      ) fs (best,size)
  in
  aux fs (Int.max_int,0) |> snd

let run f =
  let input = Utils.read_all f in
  match Angstrom.parse_string ~consume:Angstrom.Consume.All Parser.cmds input with
  | Ok cmd ->
     let state = parse_t cmd in
     Fmt.pr "%a\n" (SMap.pp String.pp pp) state;
     Fmt.pr "p1 = %d\n" (part1 state);
     Fmt.pr "p2 = %d\n" (part2 state 30_000_000)
  | Error err -> print_endline err

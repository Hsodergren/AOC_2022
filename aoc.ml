open Cmdliner

let modules = Lib.[ (1, D1.run); (2, D2.run); (3, D3.run); (4, D4.run); (5,D5.run); (6, D6.run) ]
let day = Arg.(required & pos 0 (some int) None & info [] ~docv:"DAY")
let input = Arg.(required & pos 1 (some string) None & info [] ~docv:"FILE")
let dir = Arg.(value & opt dir "" & info [ "d"; "directory" ])
let run f path = f path

let main day input dir =
  match List.find_opt (fun (d, _) -> d = day) modules with
  | Some (_, m) ->
      let path =
        Fpath.(Fpath.v dir / string_of_int day / input) |> Fpath.to_string
      in
      if Sys.file_exists path then (
        Printf.printf "==== DAY %d ====\n" day;
        run m path)
      else Printf.printf "%s doesn't exist\n" path
  | None -> print_endline "day not found"

(* let commands = *)
(*   let days = modules |> List.map fst in *)

let () =
  ignore
  @@ Cmd.eval (Cmd.v (Cmd.info "run") Term.(const main $ day $ input $ dir))

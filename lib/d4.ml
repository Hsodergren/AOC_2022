module Range = struct
  type t = { min : int; max : int }

  module Parser = struct
    open Angstrom

    let int =
      take_while (function '0' .. '9' -> true | _ -> false) >>| int_of_string

    let range = (fun min max -> { min; max }) <$> int <* char '-' <*> int
    let pair r1 r2 = (r1, r2)
    let range2 = pair <$> range <* char ',' <*> range

    let parse str =
      parse_string ~consume:Consume.All range2 str |> Result.get_ok
  end

  let contains r1 r2 = r1.min <= r2.min && r1.max >= r2.max

  let overlaps r1 r2 =
    contains r2 r1
    || (r1.min <= r2.min && r2.min <= r1.max)
    || (r1.min <= r2.max && r2.max <= r1.max)
end

let uncurry f (a, b) = f a b

let part1 lines =
  lines |> Seq.map Range.Parser.parse
  |> Utils.count (fun (r1, r2) -> Range.contains r1 r2 || Range.contains r2 r1)

let part2 lines =
  lines |> Seq.map Range.Parser.parse |> Utils.count (uncurry Range.overlaps)

let run f =
  let seq = Utils.lines f in
  part1 seq |> Fmt.pr "%d\n";
  part2 seq |> Fmt.pr "%d\n"

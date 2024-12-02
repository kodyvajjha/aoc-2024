let _test = {|7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
|}

module Parsing = struct
  open CCParse

  let levels = sep_until ~until:endline ~by:white U.int

  let reports = many levels
end

module Solving = struct
  let rec first_difference = function
    | x1 :: (x2 :: _ as rest) -> (x2 - x1) :: first_difference rest
    | _ -> []

  let is_safe (l : int list) =
    let same_sign l =
      CCList.for_all (fun x -> x > 0) l || CCList.for_all (fun x -> x < 0) l
    in
    let within_bounds l =
      CCList.for_all (fun x -> 1 <= CCInt.abs x && CCInt.abs x <= 3) l
    in
    let diffs = first_difference l in
    CCFormat.printf "@.Original: %a   Diffs: %a   Safe: %s"
      CCFormat.Dump.(list int)
      l
      CCFormat.Dump.(list int)
      diffs
      (string_of_bool (same_sign diffs && within_bounds diffs));
    same_sign diffs && within_bounds diffs

  let num_is_safe (l : int list list) = CCList.filter is_safe l |> CCList.length
end

let () =
  let res =
    let open CCResult in
    let+ reports = CCParse.parse_string Parsing.reports _test in
    Solving.num_is_safe reports
  in
  CCFormat.printf "@.(Part 1,Part 2) : %a@." CCFormat.Dump.(CCResult.pp int) res

let () =
  let res =
    let open CCResult in
    let+ reports = CCParse.parse_file Parsing.reports "bin/day2/input.txt" in
    Solving.num_is_safe reports
  in
  CCFormat.printf "@.(Part 1,Part 2) : %a@." CCFormat.Dump.(CCResult.pp int) res

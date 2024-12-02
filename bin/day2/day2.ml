[@@@warning "-32"]

let test = {|7 6 4 2 1
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
    same_sign diffs && within_bounds diffs

  let num_is_safe (l : int list list) = CCList.filter is_safe l |> CCList.length

  let is_safe_after_removal (l : int list) =
    let len = CCList.length l in
    let removed_lists =
      CCList.map (fun i -> CCList.remove_at_idx i l) CCList.(0 -- (len - 1))
    in
    let bool_list = CCList.map is_safe removed_lists in
    CCList.exists (fun x -> x = true) bool_list

  let num_is_safe_after_removal (l : int list list) =
    CCList.filter is_safe_after_removal l |> CCList.length
end

let () =
  let res =
    let open CCResult in
    let+ reports = CCParse.parse_string Parsing.reports test in
    Solving.num_is_safe reports, Solving.num_is_safe_after_removal reports
  in
  CCFormat.printf "@.(Part 1,Part 2) : %a@."
    CCFormat.Dump.(CCResult.pp (pair int int))
    res

let () =
  let res =
    let open CCResult in
    let+ reports = CCParse.parse_file Parsing.reports "bin/day2/input.txt" in
    Solving.num_is_safe reports, Solving.num_is_safe_after_removal reports
  in
  CCFormat.printf "@.(Part 1,Part 2) : %a@."
    CCFormat.Dump.(CCResult.pp (pair int int))
    res

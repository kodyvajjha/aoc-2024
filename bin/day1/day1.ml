[@@@warning "-32"]

let test = {|
3   4
4   3
2   5
1   3
3   9
3   3
|}

module Parse = struct
  open CCParse

  let firsts =
    let* a = U.int <* white <* U.int in
    pure a

  let seconds =
    let* b = U.int *> white *> U.int in
    pure b

  let first_list =
    let* first_list = many firsts in
    pure first_list

  let second_list =
    let* second_list = many seconds in
    pure second_list
end

module Solving = struct
  let similarity first_list second_list =
    let first_list, second_list =
      ( CCList.sort CCInt.compare first_list,
        CCList.sort CCInt.compare second_list )
    in
    CCList.fold_left ( + ) 0
      (CCList.map2 (fun x y -> CCInt.abs (x - y)) first_list second_list)

  let rec freq r l i =
    match l with
    | [] -> r
    | x :: xs ->
      if i = x then
        freq (r + 1) xs i
      else
        freq r xs i

  let score first_list second_list =
    let scores = CCList.map (fun x -> x * freq 0 second_list x) first_list in
    CCList.fold_left ( + ) 0 scores
end

let () =
  let res =
    let open CCResult in
    let* first_list = CCParse.parse_string Parse.first_list test in
    let* second_list = CCParse.parse_string Parse.second_list test in
    pure
    @@ ( Solving.similarity first_list second_list,
         Solving.score first_list second_list )
  in
  CCFormat.printf "(Part 1,Part 2) : %a@."
    CCFormat.Dump.(CCResult.pp (pair int int))
    res

let () =
  let res =
    let open CCResult in
    let* first_list =
      CCParse.parse_file Parse.first_list "bin/day1/input.txt"
    in
    let* second_list =
      CCParse.parse_file Parse.second_list "bin/day1/input.txt"
    in
    pure
    @@ ( Solving.similarity first_list second_list,
         Solving.score first_list second_list )
  in
  CCFormat.printf "(Part 1,Part 2) : %a@."
    CCFormat.Dump.(CCResult.pp (pair int int))
    res

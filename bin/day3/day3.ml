[@@@warning "-32"]

let test =
  {|xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))|}

module Parsing = struct
  open CCParse

  let instr =
    let* n1 = exact "mul(" *> U.int in
    let* n2 = char ',' *> U.int in
    let* _ = char ')' in
    pure (Some (n1, n2))

  let muls =
    many
    @@ (instr
       <|> let+ _ = any_char in
           None)
end

module Solving = struct
  let sum (l : (int * int) list) =
    CCList.fold_left ( + ) 0 (CCList.map (fun (x, y) -> x * y) l)
end

let () =
  let mul_ints =
    CCParse.parse_string_exn Parsing.muls test
    |> CCList.filter CCOption.is_some
    |> CCList.map (CCOption.get_exn_or "failed")
  in
  CCFormat.printf "Part1 : %d@." (Solving.sum mul_ints)

let () =
  let mul_ints =
    CCParse.parse_file_exn Parsing.muls "bin/day3/input.txt"
    |> CCList.filter CCOption.is_some
    |> CCList.map (CCOption.get_exn_or "failed")
  in
  CCFormat.printf "Part1 : %d@." (Solving.sum mul_ints)

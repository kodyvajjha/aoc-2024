[@@@warning "-32"]

let test =
  {|xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))|}

let test2 =
  {|xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))|}

module Solving = struct
  let sum (l : (int * int) list) =
    CCList.fold_left ( + ) 0 (CCList.map (fun (x, y) -> x * y) l)

  type t =
    | Mul of int * int
    | Do
    | Dont
    | Char of char
  [@@deriving show]

  let get_enabled_muls (l : t list) =
    let rec aux enabled cur lst =
      match lst with
      | c :: cs ->
        (match (c : t) with
        | Mul (x, y) ->
          if enabled then
            aux enabled (Mul (x, y) :: cur) cs
          else
            aux enabled cur cs
        | Do -> aux true cur cs
        | Dont -> aux false cur cs
        | Char _ -> aux enabled cur cs)
      | [] -> cur
    in
    aux true [] l

  let part2_sum (l : t list) =
    let get_prod c =
      match c with
      | Mul (x, y) -> Some (x, y)
      | _ -> None
    in
    let muls = CCList.filter_map get_prod l in
    CCList.fold_left ( + ) 0 (CCList.map (fun (x, y) -> x * y) muls)
end

module Parsing = struct
  open CCParse

  module Part1 = struct
    let instr_parser =
      let* n1 = exact "mul(" *> U.int in
      let* n2 = char ',' *> U.int in
      let* _ = char ')' in
      pure (Some (n1, n2))

    let muls =
      many
      @@ (instr_parser
         <|> let+ _ = any_char in
             None)
  end

  module Part2 = struct
    open Solving

    let mul_parser =
      let* n1 = exact "mul(" *> U.int in
      let* n2 = char ',' *> U.int in
      let* _ = char ')' in
      pure (Mul (n1, n2))

    let do_parser = exact "do()" >>= fun _ -> pure Do

    let dont_parser = exact "don't()" >>= fun _ -> pure Dont

    let char_parser = any_char >>= fun c -> pure (Char c)

    let muls = many @@ (mul_parser <|> do_parser <|> dont_parser <|> char_parser)
  end
end

let () =
  let mul_ints =
    CCParse.parse_string_exn Parsing.Part1.muls test
    |> CCList.filter CCOption.is_some
    |> CCList.map (CCOption.get_exn_or "failed")
  in
  CCFormat.printf "Part1 : %d@." (Solving.sum mul_ints)

let () =
  let mul_ints =
    CCParse.parse_file_exn Parsing.Part1.muls "bin/day3/input.txt"
    |> CCList.filter CCOption.is_some
    |> CCList.map (CCOption.get_exn_or "failed")
  in
  CCFormat.printf "Part1 : %d@." (Solving.sum mul_ints)

let () =
  let mul_ints =
    CCParse.parse_string_exn Parsing.Part2.muls test2
    |> Solving.get_enabled_muls |> Solving.part2_sum
  in

  CCFormat.printf "Part2 : %d@." mul_ints

let () =
  let mul_ints =
    CCParse.parse_file_exn Parsing.Part2.muls "bin/day3/input.txt"
    |> Solving.get_enabled_muls |> Solving.part2_sum
  in

  CCFormat.printf "Part2 : %d@." mul_ints

[@@@warning "-32"]

let test =
  {|xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))|}

module Parsing = struct
  open CCParse

  let get_muls input =
    let regex = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))" in
    let rec find_matches pos acc =
      try
        let start_pos = Str.search_forward regex input pos in
        let match_str = Str.matched_string input in
        find_matches (start_pos + 1) (match_str :: acc)
      with Not_found -> List.rev acc
    in
    find_matches 0 []

  let instr =
    let* n1 = exact "mul(" *> U.int in
    let* n2 = char ',' *> U.int in
    let* _ = char ')' in
    pure (n1, n2)
end

module Solving = struct
  let sum (l : (int * int) list) =
    CCList.fold_left ( + ) 0 (CCList.map (fun (x, y) -> x * y) l)
end

(* let () =
   let mul_strings = Parsing.get_muls test in
   let mul_ints =
     CCList.map (CCParse.parse_string_exn Parsing.instr) mul_strings
   in
   CCFormat.printf "Part1 : %d@." (Solving.sum mul_ints) *)

let () =
  let test = CCIO.(read_all (open_in "bin/day3/input.txt")) in
  let mul_strings = Parsing.get_muls test in
  let mul_ints =
    CCList.map (CCParse.parse_string_exn Parsing.instr) mul_strings
  in
  CCFormat.printf "Part1 : %d@." (Solving.sum mul_ints)

let () = ()

[@@@warning "-32"]

let test =
  {|MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX|}

let spf = CCFormat.printf "@.%a" CCFormat.Dump.(array (array char))

module Solving = struct
  module Infix = struct
    let safe_get arr (x, y) =
      if x >= 0 && y >= 0 && x < Array.length arr && y < Array.length arr.(x)
      then
        Some arr.(x).(y)
      else
        None

    let ( <?> ) = safe_get
  end

  let find_in_dirs arr ~init:(r, c) ~dxy:(dx, dy) =
    let directions =
      CCList.scan_left
        (fun (x, y) (z, w) -> x + z, y + w)
        (r, c)
        (CCList.replicate 3 (dx, dy))
    in
    CCList.map (fun (x, y) -> Infix.(arr <?> (x, y))) directions
    |> CCList.filter CCOption.is_some

  let search arr ((r, c) : int * int) =
    let key = CCList.map CCOption.some [ 'X'; 'M'; 'A'; 'S' ] in
    let dxys = [ 1, 0; -1, 0; 0, 1; 0, -1; 1, 1; -1, -1; 1, -1; -1, 1 ] in
    let all_dirs =
      CCList.map (fun dxy -> find_in_dirs arr ~init:(r, c) ~dxy) dxys
    in
    CCList.filter (fun l -> l = key) all_dirs |> CCList.length

  let search_array (arr : char array array) =
    let module A = CCArray in
    let rows = A.length arr in
    let columns = A.length arr.(0) in
    let ans = ref 0 in
    for r = 0 to rows - 1 do
      for c = 0 to columns - 1 do
        ans := !ans + search arr (r, c)
      done
    done;
    !ans
end

module Parsing = struct
  let of_string s =
    let cll = CCList.(CCString.split_on_char '\n' s >|= CCString.to_list) in
    CCArray.map CCArray.of_list (CCArray.of_list cll)

  let of_filename s = CCIO.(read_all (open_in s)) |> of_string
end

(* let () =
   let open Solving.Infix in
   let open CCOption in
   let mx = Parsing.of_filename "bin/day4/input.txt" in
   let el = mx <?> (10, 0) <+> (mx <?> (11, 0)) <+> (mx <?> (12, 0)) in
   CCFormat.printf "@.%a" CCFormat.Dump.(array char) mx.(0);
   CCFormat.printf "@.%a" CCFormat.Dump.(CCFormat.some char) el *)

let () =
  let arr = Parsing.of_string test in
  let ans = Solving.search_array arr in
  CCFormat.printf "@.Part1: %d" ans

let () =
  let arr = Parsing.of_filename "bin/day4/input.txt" in
  let ans = Solving.search_array arr in
  CCFormat.printf "@.Part1: %d" ans

(* let () =
   (* let open Solving.Infix in *)
   (* let open CCOption in *)
   let mx = Parsing.of_filename "bin/day4/input.txt" in
   let el = Solving.find_in_dirs mx ~init:(0, 0) ~dxy:(-1, -1) in
   CCFormat.printf "@.%a" CCFormat.Dump.(array char) mx.(0);
   CCFormat.printf "@.%a" CCFormat.Dump.(list (CCFormat.some char)) el *)

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
    let safe_get arr x y =
      if x >= 0 && y >= 0 && x < Array.length arr && y < Array.length arr.(x)
      then
        Some arr.(x).(y)
      else
        None

    let ( <|> ) a b =
      match a with
      | Some _ -> a
      | None -> b
  end

  let check_forward arr (r, c) =
    arr.(r).(c) = 'X'
    && arr.(r + 1).(c) = 'M'
    && arr.(r + 2).(c) = 'A'
    && arr.(r + 3).(c) = 'S'

  let check_backward arr (r, c) =
    arr.(r).(c) = 'X'
    && arr.(r - 1).(c) = 'M'
    && arr.(r - 2).(c) = 'A'
    && arr.(r - 3).(c) = 'S'

  let check_top arr (r, c) =
    arr.(r).(c) = 'X'
    && arr.(r).(c + 1) = 'M'
    && arr.(r).(c + 2) = 'A'
    && arr.(r).(c + 3) = 'S'

  let check_down arr (r, c) =
    arr.(r).(c) = 'X'
    && arr.(r).(c - 1) = 'M'
    && arr.(r).(c - 2) = 'A'
    && arr.(r).(c - 3) = 'S'

  let check_top_right arr (r, c) =
    arr.(r).(c) = 'X'
    && arr.(r + 1).(c + 1) = 'M'
    && arr.(r + 2).(c + 2) = 'A'
    && arr.(r + 3).(c + 3) = 'S'

  let check_bottom_left arr (r, c) =
    arr.(r).(c) = 'X'
    && arr.(r - 1).(c - 1) = 'M'
    && arr.(r - 2).(c - 2) = 'A'
    && arr.(r - 3).(c - 3) = 'S'

  let check_top_left arr (r, c) =
    arr.(r).(c) = 'X'
    && arr.(r - 1).(c + 1) = 'M'
    && arr.(r - 2).(c + 2) = 'A'
    && arr.(r - 3).(c + 3) = 'S'

  let check_bottom_right arr (r, c) =
    arr.(r).(c) = 'X'
    && arr.(r + 1).(c - 1) = 'M'
    && arr.(r + 2).(c - 2) = 'A'
    && arr.(r + 3).(c - 3) = 'S'

  let search arr ((r, c) : int * int) = assert false

  let search_array (mx : char array array) =
    let module A = CCArray in
    let rows = A.length mx in
    let columns = A.length mx.(0) in
    for r = 0 to rows - 1 do
      for c = 0 to columns - 1 do
        ()
      done
    done
end

module Parsing = struct
  let of_string s =
    let cll = CCList.(CCString.split_on_char '\n' s >|= CCString.to_list) in
    CCArray.map CCArray.of_list (CCArray.of_list cll)

  let of_filename s = CCIO.(read_all (open_in s)) |> of_string
end

let () =
  let mx = Parsing.of_filename "bin/day4/input.txt" in
  CCFormat.printf "@.%a" CCFormat.Dump.(array char) mx.(0)

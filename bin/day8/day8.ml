[@@@warning "-32"]

let test =
  {|............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............|}

let test2 =
  {|..........
..........
..........
....a.....
..........
.....a....
..........
..........
..........
..........|}

module Solving = struct
  type grid = char array array [@@deriving show]

  type antennae = {
    freq: char;
    pos: (int * int) list;
  }
  [@@deriving show]

  type t = antennae list [@@deriving show]

  let get_antennae grid freq =
    let rows = CCArray.length grid in
    let cols = CCArray.length grid.(0) in
    let pos = ref [] in
    for r = 0 to rows - 1 do
      for c = 0 to cols - 1 do
        if grid.(r).(c) = freq then pos := !pos @ [ r, c ]
      done
    done;
    { freq; pos = !pos }

  let mk grid : t =
    let rows = CCArray.length grid in
    let cols = CCArray.length grid.(0) in
    let freqs = ref [] in
    for r = 0 to rows - 1 do
      for c = 0 to cols - 1 do
        if grid.(r).(c) != '.' then freqs := !freqs @ [ grid.(r).(c) ]
      done
    done;
    CCList.map (get_antennae grid) !freqs |> CCList.uniq ~eq:( = )

  module Part1 = struct
    let antinodes (x1, y1) (x2, y2) =
      let unit_x, unit_y = x2 - x1, y2 - y1 in
      let antinode1 = x1 - unit_x, y1 - unit_y in
      let antinode2 = x2 + unit_x, y2 + unit_y in
      antinode1, antinode2

    let is_safe arr (x, y) =
      x >= 0 && y >= 0 && x < Array.length arr && y < Array.length arr.(x)

    let solve_p1 grid (antennae : t) =
      let pos =
        let open CCList in
        let* antenna = antennae in
        let* pos1 = antenna.pos in
        let* pos2 = filter (fun x -> x != pos1) antenna.pos in
        let an1, an2 = antinodes pos1 pos2 in

        if is_safe grid an1 && is_safe grid an2 then
          [ an1; an2 ]
        else if is_safe grid an1 then
          [ an1 ]
        else if is_safe grid an2 then
          [ an2 ]
        else
          []
      in

      pos |> CCList.uniq ~eq:( = )
  end

  module Part2 = struct
    let antinodes grid (x1, y1) (x2, y2) =
      let unit_x, unit_y = x2 - x1, y2 - y1 in
      let antinode1 n = x1 - (n * unit_x), y1 - (n * unit_y) in
      let antinode2 n = x2 + (n * unit_x), y2 + (n * unit_y) in
      let p1 =
        CCList.map antinode1 CCList.(0 -- 50)
        |> CCList.filter (Part1.is_safe grid)
      in
      let p2 =
        CCList.map antinode2 CCList.(0 -- 50)
        |> CCList.filter (Part1.is_safe grid)
      in
      p1 @ p2

    let solve grid (antennae : t) =
      let pos =
        let open CCList in
        let* antenna = antennae in
        let* pos1 = antenna.pos in
        let* pos2 = filter (fun x -> x != pos1) antenna.pos in
        antinodes grid pos1 pos2
      in

      pos |> CCList.uniq ~eq:( = )
  end
end

module Parsing = struct
  let grid_of_filename s : Solving.grid = Aoclib.Parsing.grid_of_filename s

  let grid_of_string s : Solving.grid = Aoclib.Parsing.grid_of_string s
end

let () =
  let grid = Parsing.grid_of_string test in
  let antennae = Solving.mk grid in
  let ans = Solving.Part1.solve_p1 grid antennae in
  CCFormat.printf "@.Part1 : %d" (ans |> CCList.length)

let () =
  let grid = Parsing.grid_of_filename "bin/day8/input.txt" in
  let antennae = Solving.mk grid in
  let ans = Solving.Part1.solve_p1 grid antennae in
  CCFormat.printf "@.Part1 : %d" (ans |> CCList.length)

let () =
  let grid = Parsing.grid_of_string test in
  let antennae = Solving.mk grid in
  let ans = Solving.Part2.solve grid antennae in
  CCFormat.printf "@.Part2 : %d" (ans |> CCList.length)

let () =
  let grid = Parsing.grid_of_filename "bin/day8/input.txt" in
  let antennae = Solving.mk grid in
  let ans = Solving.Part2.solve grid antennae in
  CCFormat.printf "@.Part2 : %d" (ans |> CCList.length)

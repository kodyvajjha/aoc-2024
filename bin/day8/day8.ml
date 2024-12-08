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

module Solving = struct
  type grid = char array array

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

  let antinodes (x1, y1) (x2, y2) = ()
end

module Parsing = struct
  let grid_of_filename s : Solving.grid = Aoclib.Parsing.grid_of_filename s

  let grid_of_string s : Solving.grid = Aoclib.Parsing.grid_of_string s
end

let () =
  let grid = Parsing.grid_of_string test in
  let antennae = Solving.mk grid in
  CCFormat.printf "@.%a" Solving.pp antennae

let () =
  let grid = Parsing.grid_of_filename "bin/day8/input.txt" in
  let antennae = Solving.mk grid in
  CCFormat.printf "@.%a" Solving.pp antennae

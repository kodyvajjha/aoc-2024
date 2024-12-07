[@@@warning "-32"]

let test =
  {|....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...|}

module Solving = struct
  type grid = char array array

  let pp_grid fpf g =
    CCFormat.fprintf fpf "@.%a" CCFormat.Dump.(array (array char)) g

  (* type pos = int * int *)

  (* type t = {
       start: pos;
       obstructions: pos list;
       _dims: int * int;
     } *)

  let get_start grid =
    let rows = CCArray.length grid in
    let cols = CCArray.length grid.(0) in
    let pos = ref (0, 0) in
    for r = 0 to rows - 1 do
      for c = 0 to cols - 1 do
        if grid.(r).(c) = '^' then pos := r, c
      done
    done;
    !pos

  type bearing =
    | N
    | S
    | E
    | W

  let step b (x, y) =
    match b with
    | N -> x - 1, y
    | S -> x + 1, y
    | E -> x, y + 1
    | W -> x, y - 1

  let rotate (b : bearing) =
    match b with
    | N -> E
    | S -> W
    | E -> S
    | W -> N

  let simulate grid (start_x, start_y) =
    (* let open Aoclib.Infix in *)
    let rows = CCArray.length grid in
    let cols = CCArray.length grid.(0) in
    let x, y = ref start_x, ref start_y in
    let bear = ref N in
    let count = ref [ start_x, start_y ] in
    while !x != cols || !y != rows do
      CCFormat.printf "@.Pos : %a  Count : %d"
        CCFormat.Dump.(pair int int)
        (!x, !y)
        (!count |> CCList.uniq ~eq:( = ) |> CCList.length);
      let next_x, next_y = step !bear (!x, !y) in
      if grid.(next_x).(next_y) = '#' then
        bear := rotate !bear (* Turn 90 degs. *)
      else (
        count := !count @ [ !x, !y ];
        x := next_x;
        y := next_y
      )
    done;
    !count |> CCList.uniq ~eq:( = ) |> CCList.length

  let simulate grid (start_x, start_y) =
    let open Aoclib.Infix in
    let rows = CCArray.length grid in
    let cols = CCArray.length grid.(0) in
    let x, y = ref start_x, ref start_y in
    let bear = ref N in
    let count = ref [ start_x, start_y ] in
    while !x != cols || !y != rows do
      let next_x, next_y = step !bear (!x, !y) in
      match grid <?> (next_x, next_y) with
      | Some v ->
        if v = '#' then
          bear := rotate !bear (* Turn 90 degs. *)
        else (
          count := !count @ [ !x, !y ];
          x := next_x;
          y := next_y
        )
      | None ->
        CCFormat.printf "@.%d"
          ((!count |> CCList.uniq ~eq:( = ) |> CCList.length) + 1);
        exit 0
    done;
    !count |> CCList.uniq ~eq:( = ) |> CCList.length
end

module Parsing = struct
  let grid_of_string s : Solving.grid =
    let cll = CCList.(CCString.split_on_char '\n' s >|= CCString.to_list) in
    CCArray.map CCArray.of_list (CCArray.of_list cll)

  let grid_of_filename s = CCIO.(read_all (open_in s)) |> grid_of_string
end

(* let () =
   let grid = Parsing.grid_of_string test in
   let start_x, start_y = Solving.get_start grid in
   CCFormat.printf "@.Part1 : %d" (Solving.simulate grid (start_x, start_y)) *)

let () =
  let grid = Parsing.grid_of_filename "bin/day6/input.txt" in
  let start_x, start_y = Solving.get_start grid in
  CCFormat.printf "@.Part1 : %d" (Solving.simulate grid (start_x, start_y))

[@@@warning "-32-33"]

let test =
  {|190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
|}

let test2 = {|
16931: 568 2 9 16 529 1 8 1 3 8
|}

module Solving = struct
  type equation = {
    result: int;
    operands: int list;
  }
  [@@deriving show]

  type t = equation list [@@deriving show]

  module Part1 = struct
    type tree =
      | Leaf
      | Node of (int * tree * tree)
    [@@deriving show]

    let rec tree_of_list (l : int list) =
      match l with
      | x :: xs -> Node (x, tree_of_list xs, tree_of_list xs)
      | [] -> Leaf

    let is_valid equation =
      let module F = CCFormat in
      let rec dfs s tree acc =
        match tree with
        | Leaf ->
          if acc = equation.result then (
            CCFormat.printf "%s@." s;
            acc = equation.result
          ) else
            acc = equation.result
        | Node (x, ltree, rtree) ->
          dfs (s ^ F.sprintf " + %d" x) ltree (acc + x)
          || dfs (s ^ F.sprintf " * %d" x) rtree (acc * x)
      in
      (* This needs to be done as there is no uniform additive and multipliciative inverse! Wish we were in a field with one element...*)
      dfs
        (CCFormat.sprintf "%d = " equation.result)
        (tree_of_list equation.operands)
        1
      && dfs
           (CCFormat.sprintf "%d = " equation.result)
           (tree_of_list equation.operands)
           0

    let solve t =
      (* CCList.iter
         (fun eq ->
           CCFormat.printf "@.equation : %a is_valid : %s@." pp_equation eq
             (string_of_bool (is_valid eq)))
         t; *)
      CCList.(
        let+ l = filter is_valid t in
        l.result)
      |> CCList.fold_left ( + ) 0
  end
end

module Parsing = struct
  open CCParse

  let line =
    let* result = U.int <* char ':' in
    let* operands = sep ~by:space U.int in
    let+ _ = endline in
    Solving.{ result; operands }

  let input = many line
end

let () =
  let res =
    let open CCResult in
    CCParse.parse_string Parsing.input test2 >|= fun res ->
    (* CCFormat.printf "@.%a" CCFormat.(list Solving.pp_equation) res; *)
    Solving.Part1.solve res
  in
  CCFormat.printf "@.Part 1: %a" (CCResult.pp CCFormat.int) res

let () =
  let res =
    let open CCResult in
    CCParse.parse_file Parsing.input "bin/day7/input.txt" >|= fun res ->
    (* CCFormat.printf "@.%a" CCFormat.(list Solving.pp_equation) res; *)
    Solving.Part1.solve res
  in
  CCFormat.printf "@.Part 1: %a" (CCResult.pp CCFormat.int) res

(* let () =
   let res =
     let open CCResult in
     CCParse.parse_string Parsing.input test >|= fun res -> res
   in
   CCFormat.printf "Equation : %a" (CCResult.pp Solving.pp) res *)
(* 1620690235709 *)

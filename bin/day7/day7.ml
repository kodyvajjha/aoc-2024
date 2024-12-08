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

  module Arith = struct
    (* This needs to be done as there is no uniform additive and multipliciative inverse! Wish we were in a field with one element...*)
    type nint =
      | Null
      | Value of int
    [@@deriving show]

    let add x y =
      match x, y with
      | Null, _ -> y
      | _, Null -> x
      | Value a, Value b -> Value (a + b)

    let multiply x y =
      match x, y with
      | Null, _ -> y
      | _, Null -> x
      | Value a, Value b -> Value (a * b)

    let concat x y =
      match x, y with
      | Null, _ -> y
      | _, Null -> x
      | Value a, Value b ->
        Value (int_of_string (string_of_int a ^ string_of_int b))

    let ( <?+> ) = add

    let ( <?*> ) = multiply

    let ( <?||> ) = concat
  end

  module Part1 = struct
    type tree =
      | Leaf
      | Node of (Arith.nint * tree * tree)
    [@@deriving show]

    let rec tree_of_list (l : int list) =
      match l with
      | x :: xs -> Node (Arith.Value x, tree_of_list xs, tree_of_list xs)
      | [] -> Leaf

    let is_valid equation =
      let open Arith in
      let rec dfs tree acc =
        match tree with
        | Leaf -> acc = Value equation.result
        | Node (x, ltree, rtree) ->
          dfs ltree (acc <?+> x) || dfs rtree (acc <?*> x)
      in
      dfs (tree_of_list equation.operands) Null

    let solve t =
      CCList.(
        let+ l = filter is_valid t in
        l.result)
      |> CCList.fold_left ( + ) 0
  end

  module Part2 = struct
    type tree =
      | Leaf
      | Node of (Arith.nint * tree * tree * tree)

    let rec tree_of_list (l : int list) =
      match l with
      | x :: xs ->
        Node (Arith.Value x, tree_of_list xs, tree_of_list xs, tree_of_list xs)
      | [] -> Leaf

    let is_valid equation =
      let module F = CCFormat in
      let open Arith in
      let rec dfs tree acc =
        match tree with
        | Leaf -> acc = Value equation.result
        | Node (x, ltree, mtree, rtree) ->
          dfs ltree (acc <?+> x)
          || dfs mtree (acc <?||> x)
          || dfs rtree (acc <?*> x)
      in
      dfs (tree_of_list equation.operands) Null

    let solve t =
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
    CCParse.parse_string Parsing.input test >|= fun res ->
    Solving.Part1.solve res
  in
  CCFormat.printf "@.Part 1: %a" (CCResult.pp CCFormat.int) res

let () =
  let res =
    let open CCResult in
    CCParse.parse_file Parsing.input "bin/day7/input.txt" >|= fun res ->
    Solving.Part1.solve res
  in
  CCFormat.printf "@.Part 1: %a" (CCResult.pp CCFormat.int) res

let () =
  let res =
    let open CCResult in
    CCParse.parse_string Parsing.input test >|= fun res ->
    Solving.Part2.solve res
  in
  CCFormat.printf "@.Part 2: %a" (CCResult.pp CCFormat.int) res

let () =
  let res =
    let open CCResult in
    CCParse.parse_file Parsing.input "bin/day7/input.txt" >|= fun res ->
    Solving.Part2.solve res
  in
  CCFormat.printf "@.Part 2: %a" (CCResult.pp CCFormat.int) res

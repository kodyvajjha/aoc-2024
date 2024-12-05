[@@@warning "-32-33"]

let test =
  {|47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47|}

let test2 = {|75,47,61,53,29
97,61,53,29,13
|}

module Solving = struct end

module Parsing = struct
  open CCParse

  let rule =
    let* n1 = U.int <* skip_space in
    let* _ = char '|' <* skip_space in
    let* n2 = U.int in
    pure (n1, n2)

  let rules = many rule

  let update = many (U.int <* (char ',' <|> endline))

  let updates = many update

  let input =
    let* rs = rules in
    (* let* _ = endline in *)
    (* let* _ = endline in *)
    let* ups = updates in
    pure (rs, ups)
end

let () =
  let res = CCParse.parse_string Parsing.update test2 in
  CCFormat.printf "@.%a"
    (CCResult.pp
       CCFormat.Dump.(
         (* pair *)
         (* CCFormat.Dump.(list (pair int int)) *)
         CCFormat.Dump.(list int)))
    res

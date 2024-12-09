[@@@warning "-32"]

let test = {|2333133121414131402|}

module Solving = struct
  type file = {
    id: int;
    size: int;
  }

  type repr =
    | File of file
    | Free of int

  type t = repr list

  let rec pp fpf (t : t) =
    match t with
    | e :: es ->
      (match e with
      | File { id; size } ->
        for _ = 0 to size - 1 do
          CCFormat.fprintf fpf "%d" id
        done
      | Free i ->
        if i = 0 then
          ()
        else
          for _ = 0 to i - 1 do
            CCFormat.fprintf fpf "."
          done);
      pp fpf es
    | [] -> ()
end

module Parsing = struct
  open Solving

  let char_to_int c = int_of_string @@ CCChar.to_string c

  let file_of_string str : t =
    let ans = ref [] in
    CCString.iteri
      (fun i c ->
        if CCInt.(i mod 2) = 0 then (
          let el : repr = File { id = i / 2; size = char_to_int c } in
          ans := !ans @ [ el ]
        ) else (
          let el = Free (char_to_int c) in
          ans := !ans @ [ el ]
        ))
      str;
    !ans
end

let () =
  let parsed = Parsing.file_of_string test in
  CCFormat.printf "Original : %s@.Parsed: %a" test Solving.pp parsed

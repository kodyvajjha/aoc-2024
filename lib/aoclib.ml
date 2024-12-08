module Infix = struct
  let safe_get arr (x, y) =
    if x >= 0 && y >= 0 && x < Array.length arr && y < Array.length arr.(x) then
      Some arr.(x).(y)
    else
      None

  let ( <?> ) = safe_get
end

module Parsing = struct
  let grid_of_string s =
    let cll = CCList.(CCString.split_on_char '\n' s >|= CCString.to_list) in
    CCArray.map CCArray.of_list (CCArray.of_list cll)

  let grid_of_filename s = CCIO.(read_all (open_in s)) |> grid_of_string
end

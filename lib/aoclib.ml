module Infix = struct
  let safe_get arr (x, y) =
    if x >= 0 && y >= 0 && x < Array.length arr && y < Array.length arr.(x) then
      Some arr.(x).(y)
    else
      None

  let ( <?> ) = safe_get
end

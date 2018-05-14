
type move =
  | Create of int * int
  | Move of int * int
  | Surrender
  | Pass
  | Help
  | Invalid of string
  | Score
  | Display
  | End_m


let parse_move s =
  let cmd = String.lowercase_ascii s in

  if cmd = "end" then
    End_m

  else if cmd = "show board" then
    Display

  else if cmd = "help" then
    Help

  else if cmd = "surrender" then
    Surrender

  else if cmd = "pass" then
    Pass

  else if cmd = "score" then
    Score

  else if (String.sub cmd 0 6 = "create") then
    let constants = String.sub cmd 7 ((String.length cmd) - 7) in
    let space = String.index constants ' ' in
    let s1 = String.sub constants 0 space in
    let s2 = String.sub constants
        (space + 1) ((String.length constants) - (space+1)) in
    let (i1, i2) = (int_of_string_opt s1, int_of_string_opt s2) in
    let (size, handicap) =
      match (i1, i2) with
      | (Some s, Some h) -> (s, h)
      | (None, Some h) -> (-1, 0)
      | (Some s, None) -> (0, -1)
      | (None, None) -> (-1, -1)
    in
    match (size, handicap) with
    | (-1, 0) -> Invalid ("size of board must be an integer")
    | (0, -1) -> Invalid ("number of handicap stones must be an integer")
    | (-1, -1) -> Invalid ("size + handicap stones must be integers")
    | _ -> Create (size, handicap)

  else if (String.sub cmd 0 5 = "place") then
    let str = String.sub cmd 6 ((String.length cmd) - 6) in
    let space = String.index str ' ' in
    let s1 = String.sub str 0 space in
    let s2 = String.sub str (space + 1) ((String.length str) - (space+1)) in
    let (i1, i2) = (int_of_string_opt s1, int_of_string_opt s2) in
    let (x, y) =
      match (i1, i2) with
      | (Some x', Some y') -> (x', y')
      | _ -> (-1, -1)
    in
    match (x, y) with
    | (-1, -1) -> Invalid ("position must be a valid integer location")
    | _ -> Move (x, y)
  else Invalid ("invalid move.")

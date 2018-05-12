type move =
  | Create of int
  | Move of int * int
  | Surrender
  | Score
  | Pass
  | Help
  | Invalid of string

let parse s =
  let cmd = String.lowercase_ascii s in

  if cmd = "help" then
    Help

  else if cmd = "surrender" then
    Surrender

  else if cmd = "pass" then
    Pass

  else if cmd = "score" then
    Score

  else if (String.sub cmd 0 6 = "create") then
    let str = String.sub cmd 7 ((String.length cmd) - 7) in
    let i =
      try int_of_string str
      with _ -> -1
    in
    if i = -1 then
      Invalid ("board size must be an integer")
    else
    if i = 9 || i = 13 || i = 19  then
      Create i
    else Invalid ("board size must be 9, 13, or 19.")

  else if (String.sub cmd 0 5 = "place") then
    let str = String.sub cmd 6 ((String.length cmd) - 6) in
    let space = String.index str ' ' in
    let s1 = String.sub str 0 space in
    let s2 = String.sub str (space + 1) ((String.length str) - (space+1)) in
    let i1 =
      try int_of_string s1
      with _ -> -1
    in
    let i2 =
      try int_of_string s2
      with _ -> -1
    in
    if i1 = -1 || i2 = -1 then
      Invalid ("position must be a locatin on the board")
    else
      Move (i1, i2)

  else Invalid ("invalid move.")

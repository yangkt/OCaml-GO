open Gui
open Board
open Move

type result =
  | Board of Board.t
  | Exception of string

type control = {
  previous : Board.t;
  surrender : bool
}

let init_state n h =
  if h < 5 then
    if n = 9 || n = 13 || n = 19 then
      Board (initiate_game n h)
    else
      Exception ("Invalid board size-- must be 9, 13, or 19")
  else
    Exception ("Number of handicap stones must NOT be more than 5,")

let update_gui b =
  let message = b.msg in
  if message = "Out of bounds" || message = "Position is occupied" ||
    message = "Illegal message" then
    update_message message
  else
    let p = b.player in
    let score' = score b player in
    update_score p score';
    let p = b.player in
    update_player (string_of_int p);
    update_message message;
    update_gui b

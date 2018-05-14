
open Gui
open Board
open Move

type control = {
  curr : Board.board;
  pass : bool;
  plyr : int;
  ai : bool;
}

type result =
  | Board of control
  | Exception of string
  | Help of string

let initiate_controller n h t =
  let board = Board.initiate_game n h in
  let b =
    if t = 1 then false
    else true
  in
  {curr = board; pass = false; plyr = 1; ai = b}

let init_game n h t =
  if t < 1 || t > 3 then
    Exception ("not a valid gameplay option")
  else
    if h < 0 || h > 5 then
      Exception ("Number of handicap stones must NOT be more than 5")
    else
      if n <> 9 || n <> 13 || n <> 19 then
        Exception ("Invalid board size-- must be 9, 13, or 19")
      else
        initiate_controller n h t

let update_gui board =
  let message = b.msg in
  if (message = "Out of bounds" || message = "Position is occupied" ||
      message = "Illegal move") then
    update_message message
  else
    update_message message;
    let p = b.player in
    let sp = score b p in
    let p' = ((p mod 2) + 1) in
    let sp' = score b p' in
    update_score p sp;
    update_score p' sp';
    update_player (string_of_int p);
    update_gui board

let turn s c =
  let cmd = Move.parse_move s in
  match cmd with
  | Create (s, h, t) -> initiate_controller s h t
  | Place (x, y) ->
    let board = c.curr in
    let board' = place board x y in
    {c with curr = board'; plyr = board'.player}
  | Surrender ->
    let board = c.curr in
    let board' = end_board board in
    {c with curr = board'; plyr = 0}
  | Pass ->
    let board = c.curr in
    let board' = pass board in
    {c with curr = board'; plyr = board'.player}
  | Invalid s -> Exception s
  | Help -> Help
   ("Welcome to the Game of Go. Your goal is to surround all your opponent's
    pieces by placing your own pieces there. \n
   To play, you can either ask for the current state of the board, your score or
    place a stone down at a given position. \n
   To determine the current state of the board, type 'show board'. \n
   To ask for your score, type 'score'. \n
   To place a stone down at the (x,y) corner, type 'place x y'. \n
   Scoring does not count as taking a move. Placing a stone is your turn and
    moves to the next player; however, if your move is invalid, you will be
    allowed to place a stone down again.")


    (*
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

let turn cmd b =
  let move = Move.parse cmd in
  match move with
  | Create (s, h) -> Exception "creating board in midst of game lmao"
  | Move (x, y) -> Board (place b (x, y))
  | Surrender -> Exception "lol what"
  | Score -> Exception "idk what to do ahahahaha"
  | Pass -> Exception "bleh"
  | Help -> Exception "help message HAHAHA oops"
  | Invalid s -> Exception s

*)

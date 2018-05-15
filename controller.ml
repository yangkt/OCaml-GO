open Board
open Move

type control = {
  curr : Board.board;
  pass : bool;
  ai : ai_level;
}

type result =
  | Board of control
  | Exception of string
  | Help of string
  | End of control

let initiate_controller n h t =
  let board = Board.initiate_game n h in
  let b =
    match t with
    | 1 -> None
    | 2 -> Easy
    | 3 -> Hard
    | _ -> None
  in
  Board {curr = board; pass = false; ai = b}

let init_game n h t =
  if t < 1 || t > 3 then
    Exception ("not a valid gameplay option")
  else
    if h < 0 || h > 5 then
      Exception ("Number of handicap stones must be between 0 and 5")
    else
      if n <> 9 && n <> 13 && n <> 19 then
        Exception ("Invalid board size-- must be 9, 13, or 19")
      else
        initiate_controller n h t

let get_msg control =
  let board = control.curr in
  board.msg

let score control p =
  let board = control.curr in
  score_ind board p

let get_player control =
  let board = control.curr in
  board.player

let get_stone_pos control p =
  let board = control.curr in
  get_pos board p

let rec turn s c =
  let cmd = Move.parse_move s in
  match cmd with
  | Create (s, h, t) -> init_game s h t (*need to change*)
  | Move (x, y) ->
    let board = c.curr in
    let board' = place board (x, y) in
    Board {c with curr = board'; pass = false}
  | AI_easy ->
    let board = c.curr in
    let board' = place_ai board Easy in
    Board {c with curr = board'; pass = false}
  | AI_hard ->
    let board = c.curr in
    let board' = place_ai board Hard in
    Board {c with curr = board'; pass = false}
  | Surrender ->
    let board = c.curr in
    let board' = end_board board in
      Board {c with curr = board'; pass = false}
  | Pass ->
    let board = c.curr in
    if c.pass = true then
      turn "end" c
    else
      let board' = pass board in
        Board {c with curr = board'; pass = true;}
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
    allowed to place a stone down again. \n
    To end the game, type 'end'. ")
   | Score ->
    let board = c.curr in
    let board' = score_both board in
      Board {c with curr = board';}
   | Display ->
    let board = c.curr in
    let board' = board_to_string board in
      Board {c with curr = board';}
  | End_m ->
      End c

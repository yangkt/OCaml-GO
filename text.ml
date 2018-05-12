open Board

(* NOTES:
 * - parse the command somewhere else other than in state-- will probably need
   this when the communication between/with the GUI occurs, but creating the
   command variable based on the button click
 * - deal with unvalid moves in a better way, or at least notify the player of
   what the possible command / moves are
 * - intiating player! it's really weird and lame right now but w h a t e v e r
 * - find a way to determine when the game is over
*)

type command =
  | Move of int * int
  | Score
  | Display
  | Help
  | Invalid

(* [parse str] parses the command typed into the terminal into a command type
 * returns: the type of move that the player typed in, or Invalid if the command
 * is not possible *)

let parse s =
  let cmd = String.lowercase_ascii s in

  if cmd = "help" then
    Help

  else if cmd = "show board" then
    Display

  else if cmd = "score" then
    Score

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
      Invalid
    else
      Move (i1, i2)

  else
    Invalid

(* [update b c p] updates board [b] based on the given command, [cmd]. If the
 * board needs to be updated / a move is made, then the board is updated for
 * player [p]'s stones. *)
let update board cmd p =
  match cmd with
  | Move (x, y) -> place board (x, y)
  | Score | Help | Display | Invalid -> board

(* [assign x y board p] places a stone from [p] onto [board] on the corner
 * located at ([x], [y]). Determines whether or not the stone was successfully
 * placed.
 * returns: a status message describing whether the move for placing the stone
 * was invalid / move was successful. *)
let assign x y board p =
  let board' = place board (x, y) in
  let message = board'.msg in
  if message = "Position is Occupied" || message = "Out of bounds" then
    "invalid move"
  else
    "stone is placed"

(* [help_msg] returns the message if the player needs help with the game*)
let help_msg () =
  "Welcome to the Game of Go. Your goal is to surround all your opponent's
    pieces by placing your own pieces there. \n
   To play, you can either ask for the current state of the board, your score or
    place a stone down at a given position. \n
   To determine the current state of the board, type 'show board'. \n
   To ask for your score, type 'score'. \n
   To place a stone down at the (x,y) corner, type 'place x y'. \n
   Scoring does not count as taking a move. Placing a stone is your turn and
    moves to the next player; however, if your move is invalid, you will be
    allowed to place a stone down again."

(* [play_game b p] plays one of [p]'s turn based on the current board and the
 * command typed in. Repeats until game is over. *)
let rec play_game board p =
  print_endline ("Player "^string_of_int(p)^", make a move");
  print_string "> ";
  let str = read_line () in
  let cmd = parse str in
  let board' = update board cmd p in
  match cmd with
  | Move (i1, i2) ->
      let status = assign i1 i2 board p in
      if status = "invalid move" then
        (print_endline "Your move was invalid."; play_game board' p)
      else
        (print_endline ("Your stone has been placed at (" ^ (string_of_int i1) ^
        ", " ^ (string_of_int i2) ^ ")."); play_game board' p)
  | Score -> print_endline (string_of_int (score board p)); play_game board' p
  | Help -> print_endline (help_msg ()); play_game board' p
  | Display -> print_endline (board_to_string board); play_game board' p
  | Invalid -> play_game board' p


(* [main ()] starts the text REPL and allows for game play.
 * returns: unit *)
let main () =
  ANSITerminal.(print_string [red]
    "\n\nWelcome to the Game of Go.\n");
  print_endline "Pleaose enter the size of the board you wish to play on
    (9, 13, 19).\n";
  print_string  "> ";
  let n = read_line () in
  let board = initiate_game (int_of_string n) 0 in
  play_game board 1

(* this line is necessary for the text repl in order to run--
 * [let () = main ()] is similar to any other let expression, but
 * calling main returns unit. this calls [main] in order to start the REPL*)
let () = main ()

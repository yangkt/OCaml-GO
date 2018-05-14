open Board
open Move
open Controller

(* NOTES:
 * - parse the command somewhere else other than in state-- will probably need
   this when the communication between/with the GUI occurs, but creating the
   command variable based on the button click
 * - deal with unvalid moves in a better way, or at least notify the player of
   what the possible command / moves are
 * - intiating player! it's really weird and lame right now but w h a t e v e r
 * - find a way to determine when the game is over
*)

(*
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
  *)

let rec ask_type () =
  print_endline "Choose your type of game: ";
  print_endline "[1]. 2 player \n [2]. 1 player (easy) \n [3] 1 player (hard)";
  print_string "> ";
  let op = read_int_opt () in
  match op with
  | Some n ->
    if n = 1 || n = 2 || n = 3 then
      n
    else
      ask_type ()
  | None -> ask_type ()

(* [play_game b p] plays one of [p]'s turn based on the current board and the
 * command typed in. Repeats until game is over. *)
(*let rec play_game board =
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
  | Invalid -> play_game board' p*)

let rec play_game control =
  let p = control.plyr in
  print_endline ("Player " ^ string_of_int p ^ " to move.");
  print_string "> ";
  let str = read_line () in
  let result = Controller.turn str in
  match result with
  | Exception s -> print_endline "invalid move"; play_game control
  | Board c' -> print_endline (c'.board.msg); play_game c'

(* [main ()] starts the text REPL and allows for game play.
 * returns: unit *)
let main () =
  ANSITerminal.(print_string [red]
    "\n\nWelcome to the Game of Go.\n");
  let type = ask_type () in
  print_endline "Please enter the size of the board you wish to play on
    (9, 13, 19).\n";
  print_string  "> ";
  let n = read_line () in
  print_endline "How many handicapped stones do you want to start with?";
  print_string "> ";
  let h = read_line () in
  let (x, y) = (int_of_string_opt n, int_of_string_opt h) in
  let (n', h') =
    match (x, y) with
    | (Some x', Some y') -> (x', y')
    | (Some x', None) -> (x', -1)
    | (None, Some y') -> (-1, y')
    | (None, None) -> (-1, -1)
  in
  let control = init_game n' h' type in
  play_game control

(* this line is necessary for the text repl in order to run--
 * [let () = main ()] is similar to any other let expression, but
 * calling main returns unit. this calls [main] in order to start the REPL*)
let () = main ()

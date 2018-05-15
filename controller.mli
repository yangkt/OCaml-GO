open Board

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

(* [init_game] initializes a game of Go with a empty board of size [n]X[n] and
 * [h] handicapped stones pre-placed on the board. If n is 0, then the handicap
 * option was not chosen, and the game starts with an empty board.
 * [t] reppresents the type of game play
 * 1 - p vs p, 2 - player vs easy ai, 3 - player vs hard ai
 * returns: unit*)
val init_game : int -> int -> int -> result

(* [turn m b] takes another step / turn in the game given the player's move and
 * the current state of the board
 * returns: an updated board state after the player has taken the turn *)
val turn : string -> control -> result

(* [get_msg c] obtains the status message from the controller
 * returns: the current status message for the current state *)
val get_msg : control -> string

(* [score c p] calculates and obtains the score for a player [p] at a given time
 * returns: player p's current score *)
val score : control -> int -> int

(* [get_player c] determines the player whose move it is
 * returns: whether it is currently the 1st or 2nd player to move *)
val get_player : control -> int

(* [get_stone_pos c p] obtains the list of the positions of black stones (player
   1) or white stones (player 2) that have been placed
 * returns: a list of positions for the stones the player has placed on board *)
val get_stone_pos : control -> int -> (int * int) list

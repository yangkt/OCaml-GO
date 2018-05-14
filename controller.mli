
open Board

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

(* [init_game s n] initializes a game of Go with a empty board of size s*s and
 * n handicapped stones pre-placed on the board. If n is 0, then the handicap
 * option was not chosen, and the game starts with an empty board.
 * returns: unit*)
val init_game : int -> int -> int -> result

(* [update_gui b] updates the display with the updated board state after a player
 * has taken a move *)
val update_gui : Board.board -> unit

(* [turn m b] takes another step / turn in the game given the player's move and
 * rhe current state of the board
 * returns: an updated board state after the player has taken the turn *)
val turn : string -> control -> result

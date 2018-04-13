type t

(* [init_state n] initializes a game of Go with a empty board of size n*n
 * returns: unit*)
val init_state : int -> unit

(* [update_gui b] updates the display with the updated board state after a player
 * has taken a mvoe *)
val update_gui : Board.t -> unit

(* [input i] converts a GUI input into a valid Move type *)
val input : unit -> Move.t

(* [turn m b] takes another step / turn in the game given the player's move and
 * rhe current state of the board
 * returns: an updated board state after the player has taken the turn *)
val turn : Move.t -> Board.t -> Board.t


(* represents the board for go*)
type board = {
  player : int;
  board : int array array;
  msg : string
}

type ai_level = Easy | Hard

(* [not_full] checks that [brd] is not full with stones *)
val not_full : board -> bool

(* [pass] passes the player's turn *)
val pass : board -> board

(* [initiate_game n h] initiates the game.
 * creates a board of size [n] with all entries set to 0 and sets the player
 * to [1], or black. the [h] argument represents the number of handicaps that will
 * be placed at board initiation *)
val initiate_game : int -> int -> board

(* [get_pos] returns a list of positions of the pieces of a given color
 * returns empty list of no pieces are on the board *)
val get_pos : board -> int -> (int*int) list

(* [place] places a piece into [(int*int)] for the given player as indicated in the
 * board argument.
 * int int pair represents (row, column)
 * if the placement is invalid in any way, the original board is returned with
 * a descriptive message in the msg field.  *)
val place : board -> (int*int) -> board

(* [end_board] takes a board and returns an updated board with *)
val end_board : board -> board

(* [place_ai] takes a board type and the level of the ai that is going to place
 * a stone on the board.
 * This function returns a board with the updated player and state. *)
val place_ai : board -> ai_level -> board

(* [territory_score brd plr] calculates the score from purely territory on the
 * board [brd] for player [plr] *)
val territory_score : board -> int -> int

(* [stone_score brd plr] calculates the score from purely placed stones on the
 * board [brd] for player [plr] *)
val stone_score : board -> int -> int

(* [score b p] returns the current score for the given player [p] on a game with
 * a board [b] *)
val score : board -> int -> int

(* returns a string representation of the board
 * used for ascii printing  *)
val board_to_string : board -> string

(* models the set in Arrays module in OCaml but does not return unit*)
val assign : int -> int -> int -> int array array -> int array array

(* [copy_matrix m] returns a deep copy of matrix [m]*)
val copy_matrix : int array array -> int array array

val get_group : int array array -> int * int -> int * (int * int) list

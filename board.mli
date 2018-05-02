

  (* represents the board for go*)
  type board = {
   player : int;
   board : int array array;
   msg : string
  }

  (* [initiate_game n] initiates the game.
   * creates a board of size [n] with all entries set to 0 and sets the player
   * to [1], or black.
   * raises invalid_value if the size, [n], requested is not 9, 13, or 19 *)
  val initiate_game : int -> board

  (* [is_empty] checks to see if the board is empty
   * returns true if there are no pieces on the board
   * returns false if there are pieces on the board *)
  val is_empty : board -> bool

  (* [get_pos] returns a list of positions of the pieces of a given color
   * returns empty list of no pieces are on the board *)
  val get_pos : board -> int -> (int*int) list

  (* [place] places a piece into [(int*int)] for the given [player]
   * int int pair represents (row, column)
   * if the placement is invalid in any way, the original board is returned with
   * a descriptive message in the msg field.  *)
  val place : board -> int -> (int*int) -> board

  (* [score] returns the current score for the given [player] *)
  val score : board -> int -> int

  (* [take_turn] lets the [player] take their turn
   * returns an updated board with the move implemented
  val take_turn : int -> move -> t *)

  (* returns a string representation of the board
   * used for ascii printing  *)
  val board_to_string : board -> string

  (* models the set in Arrays module in OCaml but does not return unit*)
  val assign : int -> int -> int -> int array array -> int array array



  (* represents the board for go*)
  type board = {
   player : int;
   board : int array array;
   msg : string
  }

  val not_full : board -> bool

  (* [initiate_game n] initiates the game.
   * creates a board of size [n] with all entries set to 0 and sets the player
   * to [1], or black. *)
  val initiate_game : int -> board

  (* [get_pos] returns a list of positions of the pieces of a given color
   * returns an empty list of no pieces are on the board *)
  val get_pos : board -> int -> (int*int) list

  (* [place] places a piece into [(int*int)] for the given player
   * int*int  represents (row, column)
   * if the placement is invalid in any way, the original board is returned with
   * a descriptive message in the msg field.  *)
  val place : board -> (int*int) -> board

  (* [territory_score brd plr] calculates the score from purely territory on the
   * board [brd] for player [plr] *)
  val territory_score : board -> int -> int

  (* [stone_score brd plr] calculates the score from purely placed stones on the
   * board [brd] for player [plr] *)
  val stone_score : board -> int -> int

  (* [score] returns the current score for the given [player] *)
  val score : board -> int -> int

  (* returns a string representation of the board
   * used for ascii printing  *)
  val board_to_string : board -> string

  (* models the set in Arrays module in OCaml but does not return unit*)
  val assign : int -> int -> int -> int array array -> int array array

module type Board = sig

  (* represents the board for go *)
  type t

  type player = White | Black

  (* [empty n] creates an empty board of size n
   * raises invalid_value if the size, [n], requested is not 9, 13, or 19 *)
  val empty : int -> t

  (* [is_empty] checks to see if the board is empty
   * returns true if there are no pieces on the board
   * returns false if there are pieces on the board *)
  val is_empty : t -> bool

  (* [get_pos] returns a list of positions of the pieces of a given color
   * returns empty list of no pieces are on the board *)
  val get_pos : t -> player -> (int*int) list

  (* [place] places a piece into [(int*int)] for the given [player]
   * if the placement is invalid, return the original board but
   * print an error message    *)
  val place : player -> (int*int) -> t

  (* [score] returns the current score for the given [player] *)
  val score : player -> int

  (* [take_turn] lets the [player] take their turn
   * returns an updated board with the move implemented *)
  val take_turn : player -> Move.move -> t

  (* returns a string representation of the board
   * used for ascii printing  *)
  val board_to_string : t -> string

end


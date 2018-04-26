type move = unit

(*
  type command =
  | Create of int
  | Surrender
  | Score
  | Pass
  | Place of int * int

*)




(* [parse_move s] parses the given command from the input received on the board
 * into a valid move type that is used to update the state of the board.
 * returns: a valid move to update the board *)
val parse_move : string -> move

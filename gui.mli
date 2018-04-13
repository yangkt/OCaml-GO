type t

val update_player : string -> unit

val update_score : int -> unit

val update_board : Board.t -> unit

val to_command : string -> Move.move

(* [update_display b] updates what is shown on the GUI with the current and
 * updated board. It updates the current player, each player's score, and the
 * board display.
 * returns: unit
 * *)
val update_display : Board.t -> unit

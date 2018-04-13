type t

(* [update_player s] updates the name displayed in the GUI on the current
 * display
 * requires: s to be one of the player's name*)
val update_player : string -> unit

(* [update_message s] updates the message that reveals the current state /
 * describes what just happened. *)
val update_message : string -> unit

(* [update_score i] updates the score of the current player to i.
 * requires: i is correctly calculated and is a positive integer. *)
val update_score : int -> unit

(* [draw_board (s, w, b)] updates the board with size [s] displayed on the GUI
 * by drawing all the white stones in [w] and black stones in [b]*)
val draw_board : (int * (int * int) list * (int * int) list) -> unit

(* [handle_input i] deals with whatever input turns into some object to allow
 * the controller to update whatever is necessary*)
val handle_input : [some input] -> ["object"]

open Board

type t

(* [draw_board (s, w, b)] updates the board with size [s] displayed on the GUI
 * by drawing all the white stones in [w] and black stones in [b]*)
val draw_board : int -> Board.t -> unit

(* [update_player p] updates the name displayed in the GUI on the current
 * display to player p
 * requires: p to be one of the player's name*)
val update_player : string -> unit

(* [update_message m] updates the message box to m, revealing the current state
 * of the game and the latest move. *)
val update_message : string -> unit

(* [update_score s p] updates the score of the player p to s.
 * requires: s is correctly calculated and is a positive integer; p is either 0
 * or 1*)
val update_score : int -> int -> unit

(* [update_gui b] redraws the GUI to the updated board
 * returns: unit*)
val update_gui : Board.t -> unit

(* [handle_input i] deals with whatever input. Based on the input (i.e. button
 * clicked), generates a string that represents the action.*)
val handle_input : [some input] -> string

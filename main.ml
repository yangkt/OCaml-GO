open Controller
open Gui
open Text

let rec start_game () =
  ANSITerminal.(print_string [cyan]
    "\nGraphics or text? \n [1] for graphics; [2] for text");
  let response = read_line () in
  let i = int_of_string_opt response in
  match i with
  | Some n ->
    if n = 1 then
      true
    else
      if n = 2 then
        true
      else
        false
  | None -> start_game ()

let rec main () =
  ANSITerminal.(print_string [cyan]
    "\n\nWelcome to the Game of Go, a CS 3110 Project\n");
  start_game ()

open Graphics

(* NOTES:
 * grid sizes: (sorry i hate floating point division)
  if 9x9 -> 648
    13x13 -> 650
    19x19 -> 665
 *)

(*so the gui doesn't close in 2 seconds*)
let rec run b =
  if b then run b else false

(*draw_rect draws a rectangle at (x,y) with width w and height h.*)
let draw_rect x y w h =
  draw_rect x y w h

(* [draw_grid gs s num] draws the lines for a [s]x[s] grid. [gs] the size of the
   drawing area for the grid. [num] is the number of lines that have already
   been drawn to the board.*)
let rec draw_grid gs size num =
  if num = 8 then
    ()
  else
    let interval = gs / size in
    let x = 225 + ((num + 1) * interval) in
    let y = (65+gs) - ((num + 1) * interval) in
    moveto x (65+gs);
    lineto x 65;
    moveto 225 y;
    lineto (225+gs) y;
    draw_grid gs size (num+1)

let main () =
  let () = Random.self_init () in
  open_graph " 1100x750";
  set_window_title "Settlers of Clarktan by Yuchen Shen, Yishu Zhang, \
                             Esther Jun";
  (*let bgc = rgb 44 206 238 in
  set_color bgc;
  clear_graph ();
  set_color white;*)
  let gs = 648 in
  draw_rect 225 65 gs gs;
  draw_grid gs 9 0;
  run true
(**let _ = Sys.command("clear") in
   ANSITerminal.(print_string [red] "Welcome to the Settlers of Clarktan.");
   print_newline ()*)
let _ = main ()

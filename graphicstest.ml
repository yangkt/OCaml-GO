open Graphics

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
  if num = (size - 2) then
    ()
  else
    let interval = gs / (size - 1) in
    let x = 260 + ((num + 1) * interval) in
    let y = (160+gs) - ((num + 1) * interval) in
    moveto x (160+gs);
    lineto x 160;
    moveto 260 y;
    lineto (260+gs) y;
    draw_grid gs size (num+1)

let draw_log () =
  draw_rect 200 5 696 120;
  moveto 210 131;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--18-*-*-*-*-*-iso8859-1";
  draw_string "MESSAGE LOG"

let draw_names (p1, p2) =
  Graphics.set_font "-*-fixed-medium-r-semicondensed--21-*-*-*-*-*-iso8859-1";
  moveto 85 580;
  draw_string (p1 ^ "'s score: ");
  moveto 925 580;
  draw_string (p2 ^ "'s score: ")

let rec handle_event () =
  let status = wait_next_event ([Button_down]) in
  let (x, y) = (status.mouse_x, status.mouse_y) in
  print_endline ("(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")");
  handle_event ()

let main () =
  let () = Random.self_init () in
  open_graph " 1100x750";
  set_window_title "Settlers of Clarktan by Yuchen Shen, Yishu Zhang, \
                             Esther Jun";
  (*let bgc = rgb 44 206 238 in
  set_color bgc;
  clear_graph ();
  set_color white;*)
  let gs = 576 in
  draw_rect 260 160 gs gs;
  draw_grid gs 19 0;
  draw_log ();
  draw_names ("p1", "p2");
  handle_event ()
(**let _ = Sys.command("clear") in
   ANSITerminal.(print_string [red] "Welcome to the Settlers of Clarktan.");
   print_newline ()*)
let _ = main ()

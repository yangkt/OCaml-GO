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

(*********************************************************************)
(*********   ANY AND ALL THINGS RELATING TO FINAL SCREEN   ***********)
(*********************************************************************)

(* draws the message on the final screen based on the players' scores*)
let draw_final_msg score1 score2 =
  Graphics.set_font "-*-fixed-medium-r-semicondensed--100-*-*-*-*-*-iso8859-1";
  set_color (rgb 0 0 0);

  moveto (1100/2-500) (750/2+100);
  if (score1>score2) then
    draw_string "PLAYER 1 WINS"
  else if (score1<score2) then
    draw_string "PLAYER 2 WINS"
  else
    draw_string "TIE"

(* what happens when you click restart*)
let rec get_press_final () =
  if (button_down ()) then let (x_pos, y_pos) = mouse_pos () in
    if (x_pos > 1100/2-50 && x_pos < 1100/2+50 &&
        y_pos>750/2-200 && y_pos<750/2-150)
    then clear_graph () else get_press_final ()
  else get_press_final ()


let rec draw_final_screen score1 score2 =
  clear_graph ();

  set_color (rgb 44 206 238);
  fill_rect (1100/2-50) (750/2-200) 100 50;

  draw_final_msg score1 score2;
  set_color (rgb 0 0 0);
  Graphics.set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  moveto (1100/2-40) (750/2-190);
  draw_string "Restart";
  get_press_final ()

(*********************************************************************)
(******   ANY AND ALL THINGS RELATING TO EVENTS ON GRID SCREEN   *****)
(*********************************************************************)

(* determine coordinates on based on the click
 * if in a radius (1/3 interval / size of each box), then determine and return
 * the coordinates of the click
 * returns: (-1, -1) if doesn't count as a click on a point, otherwise coords *)
let pixel_to_coord px py s =
  let interval = 576 / (s-1) in
  let (x', y') = (px-260, py-160) in
  let diff = (float_of_int interval) /. 3. in
  let x'' = (float_of_int x') /. (float_of_int interval) in
  let y'' = (float_of_int y') /. (float_of_int interval) in
  let xf = (floor x'') in
  let xc = (ceil x'') in
  let yf = (floor y'') in
  let yc = (ceil y'') in
  if (x'' -. xf <= diff && y'' -. yf <= diff) then
    (int_of_float xf, int_of_float yf)
  else
  if (abs_float (x'' -. xc)) <= diff && (abs_float (y'' -. yc)) <= diff then
    (int_of_float xc, int_of_float yc)
  else (-1, -1)

let rec handle_input n =
  let status = wait_next_event ([Button_down]) in
  let (x, y) = (status.mouse_x, status.mouse_y) in
  print_endline ("(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")");
  if (x>1100/2+400 && x<1100/2+500 && y>750/2-340 && y<750/2-290) then
    draw_final_screen 1 0
  else
  if x >= 260 && x <= 836 && y >= 160 && y <= 576 then
    handle_input n
  else
    handle_input n

let coord_to_pixel size r c =
  let interval = 576 / size in
  let x = 260 + (interval * r) in
  let y = 736 - (interval * c) in
  (x, y)

let update_gui size w b =
  set_color white;
  let interval = 576 / size in
  match w with
  | [] -> (); set_color black;
  | (r, c)::t ->
    let (x, y) = coord_to_pixel size r c in
    fill_circle x y (interval / 3);
    match b with
    | [] -> ()
    | (r, c)::t ->
      let (x, y) = coord_to_pixel size r c in
      fill_circle x y (interval / 3)

(*********************************************************************)
(******   ANY AND ALL THINGS RELATING TO EVENTS ON GRID SCREEN   *****)
(*********************************************************************)

let update_player s =
  set_color (rgb 0 0 0);
  Graphics.set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  moveto (1100/2+300) (750/2+100); 
  draw_string s

let update_message s =
  set_color (rgb 196 156 103);
  fill_rect 201 6 695 104;
  moveto 210 98;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--17-*-*-*-*-*-iso8859-1";
  draw_string s

let update_score s p =
  failwith "Unimplemented"


(*********************************************************************)
(******   ANY AND ALL THINGS RELATING GRID SCREEN DRAWINGS   *********)
(*********************************************************************)

let draw_finish () =
  set_color (rgb 44 206 238);
  fill_rect (1100/2+400) (750/2-340) 100 50;
  moveto (1100/2+410) (750/2-330);
  set_color (rgb 0 0 0);
  Graphics.set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  draw_string "Finish"

let draw_log () =
  draw_rect 200 5 696 105;
  moveto 210 110;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--18-*-*-*-*-*-iso8859-1";
  draw_string "MESSAGE LOG"

let draw_names (p1, p2) =
  Graphics.set_font "-*-fixed-medium-r-semicondensed--21-*-*-*-*-*-iso8859-1";
  moveto 85 640;
  draw_string (p1 ^ "'s score: ");
  moveto 85 580;
  draw_string "0";
  moveto 85 540;
  draw_string (p2 ^ "'s score: ");
  moveto 85 480;
  draw_string "0"




let rec draw_nums gs size count=
  set_font "-*-fixed-medium-r-semicondensed--15-*-*-*-*-*-iso8859-1";
  if count = size then ()
  else
    let interval = gs / (size -1) in
    let x = 252 + ((count) * interval) in
    let y = (160+gs) - ((count) * interval) in
    moveto x (160+gs);
    if (count<>0) then
      draw_string (Pervasives.string_of_int count) else ();
    moveto 245 y; draw_string (Pervasives.string_of_int count);
    draw_nums gs size (count+1)

let draw_player_field () =
  set_color (rgb 0 0 0);
  set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  moveto (1100/2+300) (750/2+300);
  draw_string "CURRENT PLAYER:"

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
    draw_grid gs size (num+1);
    draw_nums gs size 0;
    draw_player_field ();
    set_color black;
    fill_circle 260 160 (interval / 3);
    set_color white;
    fill_circle 836 160 (interval / 3)

(*let rec go_back () =
  if (button_down ()) then let (x_pos, y_pos) = mouse_pos () in
    if (x_pos>1100/2-100 && x_pos<1100/2+100 && y_pos>750/4 && y_pos<750/4+50)
    then main ()
    else go_back ()*)

let new_screen n =
  clear_graph ();
  set_color (rgb 196 156 103);
  fill_rect 0 0 1100 750;
  set_color (rgb 46 256 2);
  set_color black;
  let gs = 576 in
  draw_rect 260 160 gs gs;
  draw_grid gs n 0;
  draw_log ();
  draw_finish ();
  draw_names ("p1", "p2");
  handle_input n
(*fill_rect (1100/2 - 100) (750/4) 200 50;
  moveto (1100/2-50) (750/4+5);
  draw_string "Back"*)

(*********************************************************************)
(************   ANY AND ALL THINGS RELATING TO INITIALS   ************)
(*********************************************************************)

let rec show_handicap_options size =
  clear_graph ();
  (*open_graph (" 1100 750");*)
  set_color (rgb 44 206 238);
  fill_rect (1100/2) (750/2) 100 50


let rec get_press () =
  if (button_down ()) then let (x_pos, y_pos) = mouse_pos () in
    if ((x_pos>1100/2-400 && x_pos<1100/2 && y_pos>750/4+325 && y_pos<750/4+375)||
        (x_pos>1100/2+200 && x_pos<1100/2+400 && y_pos>750/4+325 && y_pos<750/4+375))
    then
      new_screen 9
    else if
      ((x_pos>1100/2-340 && x_pos<1100/2 && y_pos>750/4+150 && y_pos<750/4+200)||
       (x_pos>1100/2+200 && x_pos<1100/2+400 && y_pos>750/4+150 && y_pos<750/4+200))
    then
      new_screen 13
    else if
      ((x_pos>1100/2-400 && x_pos<1100/2 && y_pos>750/4-25 && y_pos<750/4+25)||
       (x_pos>1100/2+200 && x_pos<1100/2+400 && y_pos>750/4-25 && y_pos<750/4+25))
    then
      new_screen 19
    else if
      (x_pos>1100/2-100 && x_pos<1100/2+100 && y_pos>750/4+325 && y_pos<750/4+375)
    then
      show_handicap_options 9
    else if
      (x_pos>1100/2-100 && x_pos<1100/2+100 && y_pos>750/4+150 && y_pos<750/4+200)
    then
      show_handicap_options 13
    else if
      (x_pos>1100/2-100 && x_pos<1100/2+100 && y_pos>750/4-25 && y_pos<750/4+25)
    then
      show_handicap_options 19
    else get_press ()
  else get_press ()

let main () =
  let () = Random.self_init () in
  open_graph " 1100x750";

  set_window_title "GO";
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  moveto (1100/2-15) (750-80);
  draw_string "GO";
  set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  moveto (1100/2-50) (750-150);
  draw_string "MAIN MENU";
  set_color (rgb 44 206 238);
  fill_rect (1100/2 - 400) (750/4+325) 200 50;
  fill_rect (1100/2 - 100) (750/4+325) 200 50;
  fill_rect (1100/2 + 200) (750/4+325) 200 50;

  fill_rect (1100/2 - 400) (750/4+150) 200 50;
  fill_rect (1100/2 - 100) (750/4+150) 200 50;
  fill_rect (1100/2 + 200) (750/4+150) 200 50;

  fill_rect (1100/2 - 400) (750/4-25) 200 50;
  fill_rect (1100/2 - 100) (750/4-25) 200 50;
  fill_rect (1100/2 + 200) (750/4-25) 200 50;
  set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";

  set_color (rgb 0 0 0);
  moveto (1100/2 - 380) (750/4+340);
  draw_string "9x9";
  moveto (1100/2 - 380) (750/4+165);
  draw_string "13x13";
  moveto (1100/2 - 380) (750/4-10);
  draw_string "19x19";

  moveto (1100/2 - 90) (750/4+340);
  draw_string "9x9 HANDICAP";
  moveto (1100/2 - 90) (750/4+165);
  draw_string "13x13 HANDICAP";
  moveto (1100/2 - 90) (750/4-10);
  draw_string "19x19 HANDICAP";

  moveto (1100/2 + 210) (750/4+340);
  draw_string "9x9 WITH AI";
  moveto (1100/2 +200) (750/4+165);
  draw_string "13x13 WITH AI";
  moveto (1100/2 +200) (750/4-10);
  draw_string "19x19 WITH AI";
  get_press ();


  run true

let _ = main ()

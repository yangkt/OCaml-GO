open GMain
open GdkKeysyms
open GDraw

let locale = GtkMain.Main.init ()

(* turn an int n into a string*)
let get_string n =
  Pervasives.string_of_int n

(* update the players' scores with [score1] and [score2] and display in
   [space] *)
let display_score space score1 score2 =
  let s1 = "Player 1 Score: "^(get_string score1) in
  let s2 = "Player 2 Score: "^(get_string score2) in
  GMisc.label ~text:s1 ~packing:space#add ();
  GMisc.label ~text:s2 ~packing:space#add ()

(*fill_col   drawable#set_foreground (`NAME fill_col);
*)
let draw_rectangle (drawable) ll_x ll_y tr_x tr_y =
  let width = tr_x - ll_x in
  let height = tr_y - ll_y in
  drawable#rectangle ~x:ll_x ~y:ll_y ~width:width ~height:height ~filled: true ()



(* Initial board setup *)
let init () =
  let window = GWindow.window ~width:1000 ~height:700
      ~title:"Go" () in
  let vbox = GPack.vbox ~packing:window#add () in
  window#connect#destroy ~callback:Main.quit;
  let hbox_score = GPack.hbox ~packing:vbox#add in
  vbox#connect#destroy  ~callback:Main.quit;

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in
  let board_menu = factory#add_submenu "Board Options" in
  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "Quit" ~key:_Q ~callback: Main.quit;

 (* Score display *)
  display_score (hbox_score) (0) (0);

  (*Board display*)
  (*let drawable = GObj.drawable ~packing:vbox#pack () in
  draw_rectangle drawable 0 0 400 400;*)


  let bsize = new GMenu.factory board_menu ~accel_group in
  bsize#add_item "9 x 9" ~key:_1 ~callback: (fun () -> prerr_endline "9 x 9");
  bsize#add_item "13 x 13" ~key:_2 ~callback: (fun () -> prerr_endline "13 x 13");
  bsize#add_item "19 x 19" ~key:_3 ~callback: (fun () -> prerr_endline "19 x 19");
  bsize#add_item "Handicap Mode" ~key:_H ~callback: (fun () -> prerr_endline "Handicap Mode");


  (* Buttons for board size*)
  let button1 = GButton.button ~label:"9 x 9"
      ~packing:vbox#add () in
  button1#connect#clicked ~callback: (fun () -> prerr_endline "9 x 9");

  let button2 = GButton.button ~label:"13 x 13"
      ~packing:vbox#add () in
  button2#connect#clicked ~callback: (fun () -> prerr_endline "13 x 13");

  let button3 = GButton.button ~label:"19 x 19"
      ~packing:vbox#add () in
  button3#connect#clicked ~callback: (fun () -> prerr_endline "19 x 19");

  let button4 = GButton.button ~label:"Handicap" ~packing:vbox#add () in
  button4#connect#clicked ~callback: (fun () -> prerr_endline "Handicap Mode");


  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()
(* end init *)


let update_player p =
  failwith "Unimplemented"


let update_message m =
  failwith "Unimplemented"

let update_score s =
  failwith "Unimplemented"

let draw_board arr =
  failwith "Unimplemented"

let handle_input i  msg=
  failwith "Unimplemented"

let () = init ();

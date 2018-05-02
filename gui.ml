open GMain
open GdkKeysyms
open GObj
open Boardgui

let locale = GtkMain.Main.init ()


let update_player p =
  failwith "Unimplemented"

let update_message m =
  failwith "Unimplemented"

  let update_score s =
    failwith "Unimplemented"

let draw_board arr =
  failwith "Unimplemented"

let handle_input i =
    failwith "Unimplemented"


let main () =
  let window = GWindow.window ~width:1000 ~height:700
      ~title:"Go" () in
  let vbox = GPack.vbox ~packing:window#add () in
  window#connect#destroy ~callback:Main.quit;

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in
  let board_menu = factory#add_submenu "Board Size" in
  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "Quit" ~key:_Q ~callback: Main.quit;
  let bsize = new GMenu.factory board_menu ~accel_group in
  bsize#add_item "9 x 9" ~key:_1 ~callback: (fun () -> prerr_endline "9 x 9");
  bsize#add_item "13 x 13" ~key:_2 ~callback: (fun () -> prerr_endline "13 x 13");
  bsize#add_item "19 x 19" ~key:_3 ~callback: (fun () -> prerr_endline "19 x 19");



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




   (*let boardgui =new boardgui font ~packing:vbox#add array in
     boardgui#set_title "Random data";*)


  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = main ()
open Board
open OUnit2

let empty_9 = Array.make_matrix 9 9 0
let brd9 = {
  board = empty_9;
  player = 1;
  msg = ""
}
let empty_13 = Array.make_matrix 13 13 0
let brd13 = {
  board = empty_13;
  player = 1;
  msg = ""
}
let empty_19 = Array.make_matrix 19 19 0
let brd19= {
  board = empty_19;
  player = 1;
  msg = ""
}

let b = Array.make_matrix 9 9 0
(* (0, 0), (1, 1), (2, 2), (3, 3), (4,4), (5,5) are all 1 *)
let b1 = b |> assign 0 0 1 |> assign 1 1 1 |> assign 2 2 1
           |> assign 3 3 1 |> assign 4 4 1 |> assign 5 5 1
let get_pos_1_b1 = [(5,5);(4,4);(3,3);(2,2);(1,1);(0,0)]
let get_pos_2_b1 = []
let get_pos_2_b1_size = 75


let brd1 = {
             board = b1;
             player = 1;
             msg = ""
           }
let outofBound = "Out of bounds"
let placeMsg = "Stone placed"
let failMsg = "Position is occupied"

let get_pos_place = [(5,5);(4,4);(3,3);(2,2);(1,1);(0,1);(0,0)]


let board_test = [
(******************************************************************************
    Test is_empty
*******************************************************************************)
 "empty board 9" >:: (fun _ -> assert_equal true (is_empty brd9));
 "empty board 13" >:: (fun _ -> assert_equal true (is_empty brd13));
 "occupied board" >:: (fun _ -> assert_equal false (is_empty brd1));

(******************************************************************************
    Test get_pos
*******************************************************************************)
  "get_pos for wht" >:: (fun _ -> assert_equal get_pos_2_b1 (get_pos brd1 2));
  "get_pos size empty" >:: (fun _ -> assert_equal get_pos_2_b1_size (List.length (get_pos brd1 0)));
  "get_post non existant" >:: (fun _ -> assert_equal [] (get_pos brd1 3));

(******************************************************************************
    Test place
*******************************************************************************)
  "place on empty" >:: (fun _ -> assert_equal placeMsg ((place brd1 1 (0,1) ).msg ));
  "place out of bounds" >:: (fun _ -> assert_equal outofBound ((place brd1 1 (30,0)).msg) );
  "place on stone" >:: (fun _ -> assert_equal failMsg (place brd1 1 (0,0)).msg );


(******************************************************************************
    Test score
*******************************************************************************)

]


let suite =
  "Board test suite"
  >::: board_test

let _ = run_test_tt_main suite

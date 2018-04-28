open Board
open OUnit2

let empty_9 = Array.make_matrix 9 9 0
let empty_13 = Array.make_matrix 13 13 0
let empty_19 = Array.make_matrix 19 19 0

(* (0, 0), (1, 1), (2, 2), (3, 3), (4,4), (5,5) are all 1 *)
let b1 = empty_9 |> Board.assign 0 0 1 |> Board.assign 1 1 1 |> Board.assign 2 2 1
                 |> Board.assign 3 3 1 |> Board.assign 4 4 1 |> set 5 5 1
get_pos_1_b1 = [(5,5);(4,4);(3,3);(2,2);(1,1);(0;0)]
get_pos_2_b1 = []
get_pos_2_b1_size = 12


let brd1 = {
                player = 1;
                board = b1;
                msg = ""
              }
let outofBound = "Out of bounds"
let placeMsg = "Stone placed"
let failMsg = "Position is occupied"

get_pos_place = [(5,5);(4,4);(3,3);(2,2);(1,1);(0,1);(0;0)]


let board_test = [
(******************************************************************************
    Test is_empty
*******************************************************************************)
 "empty board 1" >:: (fun _ -> assert_equal true (empty_9));
 "empty board 1" >:: (fun _ -> assert_equal true (Board.create 9));
 "empty board 1" >:: (fun _ -> assert_equal false (empty_9));
 "empty board 1" >:: (fun _ -> assert_equal true (empty_9));

(******************************************************************************
    Test get_pos
*******************************************************************************)
  "empty board 1" >:: (fun _ -> assert_equal true (empty_9));
  "get_pos for wht" >:: (fun _ -> assert_equal get_pos_2_b1 (get_pos b1 2));
  "get_pos size empty" >:: (fun _ -> assert_equal get_pos_2_b1_size List.length (get_pos b1 0));
  "get_post non existant" >:: (fun _ -> assert_equal 0 (get_pos b1 3));

(******************************************************************************
    Test place
*******************************************************************************)
  "place on empty" >:: (fun _ -> assert_equal placeMsg (place brd1 1 (0,1).msg ));
  "place out of bounds" >:: (fun _ -> assert_equal outofBound (place brd1 1 (30,0)).msg );
  "place on stone" >:: (fun _ -> assert_equal failMsg (place brd1 1 (0,0)).msg );


(******************************************************************************
    Test score
*******************************************************************************)

]


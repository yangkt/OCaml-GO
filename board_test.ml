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

let f_brd9 = {
  board = (Array.make_matrix 9 9 1);
  player = 1;
  msg = ""
}

let b = Array.make_matrix 9 9 0
(* (0, 0), (1, 1), (2, 2), (3, 3), (4,4), (5,5) are all 1 *)
let b1 = b |> assign 0 0 1 |> assign 1 1 1 |> assign 2 2 1
           |> assign 3 3 1 |> assign 4 4 1 |> assign 5 5 1
let get_pos_1_b1 = [(5,5);(4,4);(3,3);(2,2);(1,1);(0,0)]
let get_pos_2_b1 = []
let get_pos_0_b1_size = 75

let brd1 = {
             board = b1;
             player = 1;
             msg = ""
           }

let terr_b = Array.make_matrix 9 9 0
let terr_b1 = terr_b |> assign 0 0 1 |> assign 0 4 1 |> assign 0 5 1 |>
              assign 1 3 1 |> assign 1 4 1 |> assign 2 2 1 |>
              assign 2 3 1 |> assign 3 0 1 |> assign 3 1 1 |>
              assign 3 3 1 |> assign 4 3 1 |> assign 5 3 1 |>
              assign 6 3 1 |> assign 6 7 1 |> assign 6 8 1 |>
              assign 7 2 1 |> assign 7 3 1 |> assign 7 6 1 |>
              assign 7 7 1 |> assign 8 2 1 |> assign 8 6 1 |>
              assign 0 2 2 |> assign 0 6 2 |> assign 1 1 2 |>
              assign 1 2 2 |> assign 1 5 2 |> assign 1 6 2 |>
              assign 2 0 2 |> assign 2 1 2 |> assign 2 5 2 |>
              assign 3 5 2 |> assign 4 5 2 |> assign 5 5 2 |>
              assign 5 7 2 |> assign 5 8 2 |> assign 6 5 2 |>
              assign 6 6 2 |> assign 7 4 2 |> assign 7 5 2 |>
              assign 8 3 2 |> assign 8 4 2 |> assign 2 8 2 |>
              assign 3 8 2 |> assign 8 8 1

let brd2 = {
  board = terr_b1;
  player = 1;
  msg = ""
}

let b3 = copy_matrix b1
let brd3 = {
  board = b3;
  player = 1;
  msg = ""
}


let outofBound = "Out of bounds"
let placeMsg = "Stone placed at: (0,1)"
let failMsg = "Position is occupied"

let get_pos_place = [(5,5);(4,4);(3,3);(2,2);(1,1);(0,1);(0,0)]


let board_test = [

(******************************************************************************
    Test get_pos
*******************************************************************************)
  "get_pos for wht" >:: (fun _ -> assert_equal get_pos_2_b1 (get_pos brd1 2));
  "get_pos size empty" >:: (fun _ -> assert_equal get_pos_0_b1_size (List.length (get_pos brd1 0)));
  "get_post non existant" >:: (fun _ -> assert_equal [] (get_pos brd1 3));

(******************************************************************************
    Test place
*******************************************************************************)
  (* "place on empty" >:: (fun _ -> assert_equal placeMsg ((place brd1 (0,1) ).msg ));
  "place out of bounds" >:: (fun _ -> assert_equal outofBound ((place brd1 (30,0)).msg) );
  "place on stone" >:: (fun _ -> assert_equal failMsg (place brd1 (0,0)).msg ); *)

(******************************************************************************
    Test end of game
*******************************************************************************)
  "full board" >:: (fun _ -> assert_equal true (not_full f_brd9));
  "empty board" >:: (fun _ -> assert_equal false (not_full brd1) );

(******************************************************************************
    Test score
*******************************************************************************)
 "stone_score empty_for_black" >:: (fun _ -> assert_equal 0 (stone_score brd13 1) );
 "stone_score empty_for_white" >:: (fun _ -> assert_equal 0 (stone_score brd13 2) );
 "stone_score empty_only_for_white" >:: (fun _ -> assert_equal 0 (stone_score brd3 2) );
 "stone_score" >:: (fun _ -> assert_equal 6 (stone_score brd3 1) );
 "territory_score" >:: (fun _ -> assert_equal 16 (territory_score brd2 1));
 "territory_score" >:: (fun _ -> assert_equal 12 (territory_score brd2 2));
]


let suite =
  "Board test suite"
  >::: board_test

let _ = run_test_tt_main suite

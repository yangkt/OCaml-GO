open Gui
open Board
open Move

type t = Board.t (* TODO: change towhatever you want, I have no idea how the board
               * and the controller are going to interact so it's kind of hard
               * to determine what to make this type*)

let init_state n =
  draw_board n [] [];
  initiate_game n

let input s board =
  match Move.parse s with
  | Create n -> begin
      draw_board n [] [] (* something to do with obtaining a board after
                           * initiating it *)
    end
  | Move (x, y) -> begin
      let board' = place board 0 (x,y) in
      update_board board';
      let board' = place_board 0 (x,y) in
      update_message board'.msg;
      let score' = score 0 in
      update_score score';
    end
  | Score -> begin
      let score' = score 0 in
      update_score score';
    end
  | Surrender -> failwith "Unimplemented" (* I don't know how players are
                                           * dealt with which is why this and
                                           * the next are currently
                                           * unimplemented, because it depends
                                           * on player*)
  | Pass -> failwith "Unimpelemented"
  | Help -> failwith "Unimplemented" (* open a new window with rules lol*)
  | Invalid s -> update_message s;
  | _ -> failwith "Unimplemented"

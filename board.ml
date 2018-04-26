
   (* NOTES:
    * The element (x,y) of a matrix m is accessed with the notation m.(x).(y).
    * empty spot  = 0
      black stone = 1
      white stone = 2
  *)

    type board =
    {
      player : int;
      board : int array array;
      msg : string
    }

  (* a function to create a int array array of size [n]X[n] *)
  let create n =
    match n with
    | 9 ->  Array.make_matrix 9 9 0
    | 13 -> Array.make_matrix 13 13 0
    | 19 -> Array.make_matrix 19 19 0
    | _ -> failwith "invalid board size"

  let initiate_game n = {
      player = 1;
      board = create n;
      msg = "Game started"
    }

  let is_empty b =
    let board = b.board in
      let sum = Array.fold_left (fun x_o a_o -> x_o +
        (Array.fold_left (fun x_i l -> x_i + l) 0 a_o)) 0 board in
      match sum with
      | 0 -> true
      | _ -> false

  (* redo with array functions ? *)
  let get_pos brd plr =
    let board = brd.board in
    let lst = ref [] in
    for r = 0 to  (Array.length board)-1 do
      for c = 0 to (Array.length board)-1 do
        if board.(r).(c) = plr then
          lst := (r,c)::(!lst)
          else ()
      done;
   done;
    !lst

  let place brd plr (r, c) =
    let board = brd.board in
    match board.(r).(c) with
    | 0 -> {
             player = plr;
             board = board.(r).(c) <- plr;
             msg = "Stone placed"
           }
    | _ -> {
             player = plr;
             board = board
             msg = "Cannot place a stone here"
           }


  (* [score] returns the current score for the given [player] *)
  let score plr =
    failwith "unimplemented"

  (* [take_turn] lets the [player] take their turn
   * returns an updated board with the move implemented *)
  let take_turn plr move =
    failwith "unimplemented"

 let to_ascii i =
  match i with
  | 0 -> "."
  | 1 -> "X"
  | 2 -> "O"

(*
val map : ('a -> 'b) -> 'a array -> 'b array
Array.map f a applies function f to all the elements of a,
 and builds an array with the results returned by
 f: [| f a.(0); f a.(1); ...; f a.(Array.length a - 1) |].
    *)
  let board_to_string brd =
   let b = brd.board in
    Array.fold_left (fun s r -> s^(
      Array.fold_left (fun s_ c -> s_^" "^(to_ascii c) ) "" r )^"\r" )
    "" b





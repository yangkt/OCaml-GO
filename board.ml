
   (* NOTES:
    * The element (x,y) of a matrix m is accessed with the notation m.(x).(y).
    * empty spot  = 0
      black stone = 1
      white stone = 2

      possible changes to board
        - encapsulate the various messages in an variant
        - let the board change the player after a move
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
    | _ -> failwith "Error: invalid board size"

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

  let assign r c v a =
    Array.set a.(r) c v;
    a

  let place brd plr (r, c) =
    let board = brd.board in
    let size = Array.length board in
    if r < size && c < size then
      match board.(r).(c) with
      | 0 -> {
               player = plr;
               board = assign r c plr board;
               msg = "Stone placed"
             }
      | _ -> {
               player = plr;
               board = board;
               msg = "Position is occupied"
             }
    else
       {
         player = plr;
         board = board;
         msg = "Out of bounds"
       }

  let stone_score brd plr =
    let board = brd.board in
    let size = Array.length board in
    let int_of_bool b = if b then 1 else 0 in
    let counter = ref 0 in
    for i = 1 to size do
      for j = 1 to size do
        counter := !counter + (int_of_bool (board.(i).(j) = plr))
      done;
    done;
    !counter

  let score plr =
    failwith "unimplemented"

(*Helper function that converts a stone representation in board to a string *)
 let to_ascii i =
  match i with
  | 0 -> "."
  | 1 -> "X"
  | 2 -> "O"
  | _ -> failwith "Error: Improper representation"

  let board_to_string brd =
   let b = brd.board in
    Array.fold_left (fun s r -> s^(
      Array.fold_left (fun s_ c -> s_^" "^(to_ascii c) ) "" r )^"\r" )
    "" b


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
    for i = 0 to size - 1 do
      for j = 0 to size - 1 do
        counter := !counter + (int_of_bool (board.(i).(j) = plr))
      done;
    done;
    !counter

  let rec flood_fill board (r,c) plr count_ref =
    (* Index out of bounds *)
    if r < 0 || r >= Array.length board || c < 0 || c >= Array.length board then
      board
    else
      (* Already explored space *)
      if board.(r).(c) = -1 then
        board
      (* Empty area bordered by both black and white -> belongs to neither *)
      else if (board.(r).(c) = 1 && plr = 2) || (board.(r).(c) = 2 && plr = 1) then
        (count_ref := 0;
         board)
      (* If territory has not been claimed yet, and the current space has
       * color c, set territory to color c, and recurse *)
      else if board.(r).(c) <> 0 then
        let new_plr = if plr = 0 then board.(r).(c) else plr in
        let down = flood_fill board (r+1,c) new_plr count_ref in
        let up = flood_fill down (r-1,c) new_plr count_ref in
        let left = flood_fill up (r,c-1) new_plr count_ref in
        flood_fill left (r,c+1) board.(r).(c) count_ref
      (* Mark space as counted, increment counter, and recurse *)
      else
        let new_board = assign r c (-1) board in
        let down = flood_fill new_board (r+1,c) plr count_ref in
        let up = flood_fill down (r-1,c) plr count_ref in
        let left = flood_fill up (r,c-1) plr count_ref in
        count_ref := !count_ref  + 1;
        flood_fill left (r,c+1) board.(r).(c) count_ref

  (* Checks to see if there is an empty position in the board *)
  let contains_empty arr =
    Array.fold_left (fun acc x -> acc || Array.mem 0 x) false arr

  (* Find an empty position on board. If no empty positions, raise Not_found *)
  let find_empty arr =
    let pos = ref (-1,-1) in
    let size = Array.length arr in
    for i = 0 to size - 1 do
      for j = 0 to size - 1 do
        if arr.(i).(j) = 0 then
          pos := (i, j);
      done;
    done;
    if !pos = (-1,-1) then
      raise Not_found
    else
      !pos

  let territory_score brd plr =
    let board = brd.board in
    let size = Array.length board in
    let temp_board = ref (Array.copy board) in
    let count = ref 0 in
    while (contains_empty !temp_board = true) do
      let prev_area_count = !count in
      let pos = find_empty !temp_board in
      let new_board = flood_fill !temp_board pos 0 count in
      if (!count) - prev_area_count >= size * size / 2 then
        count := prev_area_count;
      temp_board := new_board;
    done;
    !count

  let score brd plr =
    (territory_score brd plr) + (stone_score brd plr)

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

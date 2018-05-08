
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

let initiate_game n =
  {
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

(*Helper function that converts a stone representation in board to a string *)
let to_ascii i =
  match i with
  | 0 -> "."
  | 1 -> "X"
  | 2 -> "O"
  | -1 -> "/"
  | _ -> failwith "Error: Improper representation"

let board_to_string brd =
  let b = brd.board in
  Array.fold_left (fun s r -> s^(
  Array.fold_left (fun s_ c -> s_^" "^(to_ascii c) ) "" r )^"\n" )
  "" b

let stone_score brd plr =
  List.length (get_pos brd plr)

let rec flood_fill (board, still_count) (r,c) plr count_ref =
  (* Index out of bounds *)
  if r < 0 || r >= Array.length board || c < 0 || c >= Array.length board then
    board, still_count
  else
    (* Already explored space or same colored stone already here *)
    if board.(r).(c) = -1 || board.(r).(c) = plr then
      board, still_count
    (* Empty area bordered by both black and white -> belongs to neither *)
    else if (board.(r).(c) = 1 && plr = 2) || (board.(r).(c) = 2 && plr = 1) then
      board, false
    (* Mark space as counted, increment counter, and recurse *)
    else
      let new_board = assign r c (-1) board in
      incr count_ref;
      let down = flood_fill (new_board, true) (r+1,c) plr count_ref in
      let up = flood_fill down (r-1,c) plr count_ref in
      let left = flood_fill up (r,c-1) plr count_ref in
      let right = flood_fill left (r,c+1) plr count_ref in
      right

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

let copy_matrix m =
  let n = Array.make_matrix 9 9 0 in
  for i = 0 to 8 do
    n.(i) <- Array.copy m.(i);
  done;
  n

let print_array a =
  Array.fold_left (fun s r -> s^(
    Array.fold_left (fun s_ c -> s_^" "^(to_ascii c) ) "" r )^"\n" )
  "" a

let territory_score brd plr =
  let board = brd.board in
  let size = Array.length board in
  let temp_board = copy_matrix board in
  let count = ref 0 in
  while (contains_empty temp_board) do
    let prev_count = !count in
    let pos = find_empty temp_board in
    let new_board = flood_fill (temp_board, true) pos plr count in
    if ((snd new_board) && (!count) - prev_count < size * size / 2) = false then
      count := prev_count;
  done;
  !count

let score brd plr =
  (territory_score brd plr) + (stone_score brd plr)

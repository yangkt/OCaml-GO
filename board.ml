
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

let get_pos_array arr plr =
  let lst = ref [] in
  for r = 0 to  (Array.length arr)-1 do
    for c = 0 to (Array.length arr)-1 do
      if arr.(r).(c) = plr then
        lst := (r,c)::(!lst)
      else ()
    done;
  done;
  !lst

(* redo with array functions ? *)
let get_pos brd plr =
  let board = brd.board in
  get_pos_array board plr

let get_adjacents board (row,col) =
  let neighbors = ref [] in
  if (row > 0) then
    neighbors := (row-1,col)::(!neighbors);
  if (col > 0) then
    neighbors := (row,col-1)::(!neighbors);
  if (row < (Array.length board) - 1) then
    neighbors := (row+1,col)::(!neighbors);
  if (col < (Array.length board) - 1) then
    neighbors := (row,col+1)::(!neighbors);
  !neighbors


let get_group board pos =
  let size = Array.length board in
  let color = board.(fst pos).(snd pos) in
  let visited = Array.make_matrix size size false in
  let v_list = ref [] in
  let liberties = ref 0 in
  let queue = ref [pos] in
  let rec bfs q libs v_l =
    match !q with
    | [] -> libs, v_l
    | (r,c)::t ->
      q := t;
      if visited.(r).(c) then
        bfs q libs v_l
      else
        let neighbors = get_adjacents board (r,c) in
        if board.(r).(c) = 0 then incr libs;
        if board.(r).(c) = color then
          (q := neighbors@(!q);
           v_l := (r,c)::(!v_l));
        visited.(r).(c) <- true;
        bfs q libs v_l
  in
  let (l,v) = bfs queue liberties v_list in
  !l, !v

let rec capture board grp =
  match grp with
  | [] -> ()
  | (r,c)::t ->
    board.(r).(c) <- 0;
    capture board t

let assign r c v a =
  Array.set a.(r) c v;
  let opponent = (v mod 2) + 1 in
  let neighbors = get_adjacents a (r,c) in
  let rec cap_terr l =
    match l with
    | [] -> a
    | (row,col)::t ->
      if a.(row).(col) = opponent then
        let group = get_group a (row,col) in
        if fst group = 0 then
          capture a (snd group);
        cap_terr t
      else
        cap_terr t
  in
  cap_terr neighbors

let place brd (r, c) =
  let plr = brd.player in
  let board = brd.board in
  let size = Array.length board in
  let adj = get_adjacents board (r,c) in
  let rec legal l b =
    match l with
    | [] -> b
    | (row,col)::t ->
      if board.(row).(col) = 1 || board.(row).(col) = 0 then legal t true
      else legal t b
  in
  if r < size && c < size then
    match board.(r).(c) with
    | 0 ->
      if legal adj false then
        {
          player = (plr mod 2) + 1;
          board = assign r c plr board;
          msg = "Stone placed"
        }
      else
        {
          player = plr;
          board = board;
          msg = "Illegal move"
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

(* Find positions of [plr] *)
let find_pos arr plr =
  get_pos_array arr plr

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
  while (List.length (find_pos temp_board 0) <> 0) do
    let prev_count = !count in
    let pos = List.hd (find_pos temp_board 0) in
    let new_board = flood_fill (temp_board, true) pos plr count in
    if ((snd new_board) && (!count) - prev_count < size * size / 2) = false then
      count := prev_count;
  done;
  !count

let score brd plr =
  (territory_score brd plr) + (stone_score brd plr)

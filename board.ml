
type board =
{
  player : int;
  board : int array array;
  msg : string
}

type ai_level = Easy | Hard


(********************** Game over checking functions ***************************)

(*no positions left*)
let rec not_full brd =
  let board = brd.board in
    (Array.fold_left (fun acc x -> acc || Array.mem 0 x) false board)

(*rewrites the msg field*)
let is_end_msg brd =
       {
         player = 0;
         board = brd.board;
         msg = "Game over, board is full"
       }

  (* a function to create an int array array of size [n]x[n] *)
let create n =
  match n with
  | 9 ->  Array.make_matrix 9 9 0
  | 13 -> Array.make_matrix 13 13 0
  | 19 -> Array.make_matrix 19 19 0
  | _ -> Array.make_matrix 1 1 0


let get_pos_arr arr plr =
  let lst = ref [] in
  for r = 0 to  (Array.length arr)-1 do
    for c = 0 to (Array.length arr)-1 do
      if arr.(r).(c) = plr then
        lst := (r,c)::(!lst)
      else ()
    done;
  done;
  !lst

let get_pos brd plr =
  let board = brd.board in
  get_pos_arr board plr

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

(*****************Functions that deal with board initiation******************)

(* a function to create an int array array of size [n]x[n] *)
let create n =
  match n with
  | 9 ->  Array.make_matrix 9 9 0
  | 13 -> Array.make_matrix 13 13 0
  | 19 -> Array.make_matrix 19 19 0
  | _ -> Array.make_matrix 1 1 0


(*helper function to handicap that gets the offset for adding handicap stones*)
let off_set n =
   match n with
    | 9  -> (2,n-1)
    | 13 -> (3,n-1)
    | 19 -> (3,n-1)
    | _ -> (0, 0)

(*helper function to initiate_game that places the handicap stones*)
let handicap h n =
 let (s,n') = off_set n in
 let b = create n in
 if Array.length b = 1 then b
else
  let rec hand h b =
    match h with
    | 0 -> b
    | 1 -> hand 0 (assign (s) (n'-s) 1 b )
    | 2 -> hand 1 (assign (n'-s) (s) 1 b )
    | 3 -> hand 2 (assign (n'-s) (n'-s) 1 b )
    | 4 -> hand 3 (assign (s) (s) 1 b )
    | 5 -> hand 4 (assign (n') (n') 1 b )
    | _ -> b
  in hand h b

let initiate_game n h =
 let board = handicap n h in
  {
    player = 1;
    board = board;
    msg = "Game started"
  }

(***************Functions that update board based on a turn********************)

let valid_c c n =
  c > -1 && c < n

let pass brd =
  {
   player = (brd.player mod 2) + 1 ;
   board = brd.board;
   msg = "Turn was passed"
  }


let place brd (r, c) =
  if not_full brd then
    let plr = brd.player in
    let board = brd.board in
    let size = Array.length board in
    let adj = get_adjacents board (r,c) in
    let rec legal l b =
      match l with
      | [] -> b
      | (row,col)::t ->
        if board.(row).(col) = plr || board.(row).(col) = 0 then legal t true
        else legal t b
    in
    if (valid_c r size) && (valid_c c size) then
      match board.(r).(c) with
      | 0 ->
        if legal adj false then
          {
            player = (plr mod 2) + 1;
            board = assign r c plr board;
            msg = "Stone placed at: ("^(string_of_int r)^","^(string_of_int c)^")"
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
  else is_end_msg brd


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


(************************* Scoring functions **********************************)

let stone_score_arr board plr =
  List.length (get_pos_arr board plr)

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
  get_pos_arr arr plr

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

let territory_score_arr board plr =
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

let territory_score brd plr =
  let board = brd.board in
  territory_score_arr board plr

let score_arr board plr =
  (territory_score_arr board plr) + (stone_score_arr board plr)

let score brd plr =
  (territory_score brd plr) + (stone_score brd plr)

let num_filter board (r,c) plr =
  let counter = ref 0 in
  for i = -1 to 1 do
    for j = -1 to 1 do
      if board.(i).(j) = plr then
        incr counter
    done;
  done;
  !counter

let int_of_bool b =
  if b then 1 else 0

(********************** AI place functions *********************************)

let greedy brd =
  let board = brd.board in
  let size = Array.length board in
  let temp_board = ref (copy_matrix board) in
  let score_board = Array.make_matrix 9 9 0 in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      if board.(i).(j) = 0 then
        (temp_board := assign i j 2 (!temp_board);
         score_board.(i).(j) <- (territory_score_arr !temp_board 2) +
                                (stone_score_arr !temp_board 2) +
                                (int_of_bool (num_filter board (i,j) 2 > 0)) -
                                (score_arr board 1);
         temp_board := assign i j 0 (!temp_board));
    done;
  done;
  let max_pos = ref (0,0) in
  let max_score = ref min_int in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      if (score_board.(i).(j) > !max_score) then
        (max_score := score_board.(i).(j);
         max_pos := (i,j));
    done;
  done;
  !max_pos

(*a function that returns a coordinate pair of an empty spot on a board*)
let random brd =
  let espots = get_pos brd 0 in
    let rand  = Random.int (List.length espots) in
      List.nth espots rand

let place_ai brd lvl =
  match lvl with
  | Easy -> place brd (random brd)
  | Hard -> place brd (greedy brd)

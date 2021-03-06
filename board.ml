
type board =
{
  player : int;
  board : int array array;
  msg : string
}

type ai_level = Easy | Hard | None

(********************** Game over checking functions ***************************)

(*A function that check if that are no positions left*)
let rec not_full brd =
  let board = brd.board in
    (Array.fold_left (fun acc x -> acc || Array.mem 0 x) false board)

let end_board brd =
      {
         player = 0;
         board = brd.board;
         msg = "Game over"
      }

(*A function that rewrites the msg field when the board is full*)
let is_end_msg brd =
       {
         player = 0;
         board = brd.board;
         msg = "Game over, board is full"
       }

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

let rec legal l b plr board =
  match l with
  | [] -> b
  | (row,col)::t ->
    if board.(row).(col) = plr || board.(row).(col) = 0 then legal t true plr board
    else legal t b plr board

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
let handicap n h =
 let (s,n') = off_set n in
 let b = create n in
 if Array.length b = 1 then b
else
  let rec hand h b n s =
    match h with
    | 0 -> b
    | 1 -> hand 0 (assign (s) (n-s) 1 b ) n s
    | 2 -> hand 1 (assign (n-s) (s) 1 b ) n s
    | 3 -> hand 2 (assign (n-s) (n-s) 1 b ) n s
    | 4 -> hand 3 (assign (s) (s) 1 b ) n s
    | 5 -> hand 4 (assign (n/2) (n/2) 1 b ) n s
    | _ -> Array.make_matrix 1 1 0
  in hand h b n' s

let initiate_game n h =
 let board = handicap n h in
  {
    player = 1;
    board = board;
    msg = "Game started"
  }

(************** Functions that update board based on a turn *******************)

(*A function that checks if the coordinates are valid*)
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
    if (valid_c r size) && (valid_c c size) then
      match board.(r).(c) with
      | 0 ->
        if legal adj false plr board then
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
   let m =  Array.fold_left (fun s r -> s^(
      Array.fold_left (fun s_ c -> s_^" "^(to_ascii c) ) "" r )^"\n" )
    "" b in
    {
    board = brd.board;
    player = brd.player;
    msg = m
    }

(************************* Scoring functions **********************************)

(* [stone_score_arr board plr] returns the score of [plr] attributed to placed
 * stones in [board] *)
let stone_score_arr board plr =
  List.length (get_pos_arr board plr)

let stone_score brd plr =
  List.length (get_pos brd plr)

(* [flood_fill (board, still_count) (r,c) plr count_ref] runs a flood fill
 * algorithm on [board] starting from position [(r,c)] and incrementing a
 * counter [count_ref] to calculate territory score. *)
let rec flood_fill (board, still_count) (r,c) plr count_ref =
  (* Index out of bounds *)
  if r < 0 || r >= Array.length board || c < 0 || c >= Array.length board then
    board, still_count
  else
    (* Already explored space or same colored stone already here *)
    if board.(r).(c) = -1 || board.(r).(c) = plr then
      board, still_count
    (* Empty area bordered by both black and white -> belongs to neither *)
    else if (board.(r).(c) = (plr mod 2 ) + 1) then
      board, false
    (* Mark space as counted, increment counter, and recurse *)
    else
      let new_board = assign r c (-1) board in
      incr count_ref;
      let down = flood_fill (new_board, still_count) (r+1,c) plr count_ref in
      let up = flood_fill down (r-1,c) plr count_ref in
      let left = flood_fill up (r,c-1) plr count_ref in
      let right = flood_fill left (r,c+1) plr count_ref in
      right

(* Find positions of [plr] *)
let find_pos arr plr =
  get_pos_arr arr plr

let copy_matrix m =
  let size = Array.length m in
  let n = Array.make_matrix size size 0 in
  for i = 0 to size - 1 do
    n.(i) <- Array.copy m.(i);
  done;
  n

(* [territory_score_arr board plr] returns the score of [plr] attributed
 * to territory in [board] *)
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

(* [stone_score board plr] returns the total score for [plr] in [board] *)
let score_arr board plr =
  (territory_score_arr board plr) + (stone_score_arr board plr)

let score_ind brd plr =
  (territory_score brd plr) + (stone_score brd plr)

let score_both brd =
  let score_1 = score_ind brd 1 in
  let score_2 = score_ind brd 2 in
   {
   board = brd.board;
   player = brd.player;
   msg = "Score for player 1: "^(string_of_int score_1)^
    "\n"^"Score for player 2: "^(string_of_int score_2)
   }


(********************** AI place functions *********************************)

(* [num_filter board (r,c) plr] returns the number of stones around the position
 * [(r,c)] in [board] that are the same color as [plr] in *)
let num_filter board (r,c) plr =
  let size = Array.length board in
  let counter = ref 0 in
  for i = -1 to 1 do
    for j = -1 to 1 do
      let r' = r + i in
      let c' = c + j in
      if r'>=0 && r'<size && c'>=0 && c'<size && board.(r').(c') = plr then
        incr counter
    done;
  done;
  !counter

let int_of_bool b =
  if b then 1 else 0

(* [greedy brd] runs a single step of a greedy algorithm for the AI to
 * maximize the difference between its score and the player's score. *)
let greedy brd =
  let board = brd.board in
  let size = Array.length board in
  let temp_board = ref (copy_matrix board) in
  (* Make a matrix that is the same size as [board] and have each position
   * represent the difference in score if the AI were to place a stone at that
   * position.  *)
  let score_board = Array.make_matrix size size min_int in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      if board.(i).(j) = 0 then
        (temp_board := assign i j 2 (!temp_board);
        let score = (score_arr !temp_board 2) +
                    (int_of_bool (num_filter board (i,j) 2 > 0)) -
                    (score_arr !temp_board 1) in
        score_board.(i).(j) <- score;
        (!temp_board).(i).(j) <- 0;);
    done;
  done;
  (* Find the maximum score *)
  let max_score = Array.fold_left
      (fun acc x -> Array.fold_left (fun acc_ x_ -> max acc_ x_) acc x)
      min_int score_board in
  (* Find all the positions with the maximum score *)
  let max_pos = ref [] in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      if score_board.(i).(j) = max_score then
        max_pos := (i,j)::(!max_pos);
    done;
  done;
  (* Only consider the moves that are legal *)
  let legal_moves = List.fold_left
      (fun a x ->
        let adj = get_adjacents board x in
        if legal adj false 2 board then x::a else a) [] !max_pos in
  if List.length legal_moves = 0 then
    (-1,-1)
  else
    let rand = Random.int (List.length legal_moves) in
    List.nth legal_moves rand

(* [copy_board brd] returns a deep copy of [brd] *)
let copy_board brd =
    {
    player = brd.player;
    board = copy_matrix brd.board;
    msg = brd.msg
    }

(*a function that returns a coordinate pair of an empty spot on the board*)
let random brd =
  let espots = get_pos brd 0 in
  let rec r spots =
    let rand  = Random.int (List.length espots) in
      let points = List.nth espots rand in
       let brd = place (copy_board brd) points in
       if brd.msg = "Illegal move" then
        let sp = List.filter (fun a -> a <> points) spots in
          r sp
        else points
  in r espots

let place_ai brd lvl =
  match lvl with
  | Easy ->
    Random.self_init ();
    let n = Random.int 10 in
    if n <= 1 then pass brd
    else place brd (random brd)
  | Hard ->
    let pos = greedy brd in
    if pos = (-1,-1) then pass brd
    else place brd pos
  | None -> brd

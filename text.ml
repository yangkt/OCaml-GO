
open Board
open Move
open Controller

let rec ask_type () =
  print_endline "Choose your type of game: ";
  print_endline "[1]. 2 player \n [2]. 1 player (easy) \n [3] 1 player (hard)";
  print_string "> ";
  let op = read_int_opt () in
  match op with
  | Some n ->
    if n = 1 || n = 2 || n = 3 then
      n
    else
      ask_type ()
  | None -> ask_type ()

let rec play_game control =
  let board = control.curr in
  let p = control.curr.player in
  let a = control.ai in
  match (p, a) with
  | (1, _)  | (2, None) -> begin
    print_endline ("Player " ^ string_of_int p ^ " to move.");
    print_string "> ";
    let str = read_line () in
    let result = turn str control in
    match result with
    | Help h -> print_endline h; play_game control
    | Exception s -> print_endline "invalid move"; play_game control
    | Board c -> print_endline (c.curr.msg); play_game c
    | End c -> print_endline "Thanks for playing"; exit 0
  end
  | (2, l) -> begin
      let b' = place_ai board l in
      print_endline b'.msg;
      play_game ({control with curr = b'})
    end
  | (_, _) -> print_endline "Thanks for playing"; exit 0


(* [main ()] starts the text REPL and allows for game play.
 * returns: unit *)
let rec main () =
  ANSITerminal.(print_string [red]
    "\n\nWelcome to the Game of Go.\n");
  let t = ask_type () in
  print_endline "Please enter the size of the board you wish to play on
    (9, 13, 19).\n";
  print_string  "> ";
  let n = read_line () in
  print_endline "How many handicapped stones do you want to start with?";
  print_string "> ";
  let h = read_line () in
  let (x, y) = (int_of_string_opt n, int_of_string_opt h) in
  let (n', h') =
    match (x, y) with
    | (Some x', Some y') -> (x', y')
    | (Some x', None) -> (x', -1)
    | (None, Some y') -> (-1, y')
    | (None, None) -> (-1, -1)
  in
  let control = init_game n' h' t in
  match control with
  | Board c -> play_game c
  | _ -> main ()

(* this line is necessary for the text repl in order to run--
 * [let () = main ()] is similar to any other let expression, but
 * calling main returns unit. this calls [main] in order to start the REPL*)
let _ = main ()

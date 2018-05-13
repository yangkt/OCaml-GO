Open board

 let rand brd =
    let espots = get_pos brd 0 in
    let rand  = Random.int (List.length espots) in
     List.nth espots rand

let greedy brd =
  (0,0)
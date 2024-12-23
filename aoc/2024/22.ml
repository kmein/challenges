let rec secret_number step start =
  if step <= 0 then
    start
  else
    secret_number (step - 1) (next start)
and next step0 =
  let prune x = x mod 16777216 (* 2**24 *); in
  let step1 = prune (Int.logxor step0 (Int.shift_left step0 6)); in
  let step2 = prune (Int.logxor step1 (Int.shift_right step1 5)); in
  let step3 = prune (Int.logxor step2 (Int.shift_left step2 11)); in
  step3

module StringMap = Map.Make(String)

let rec find_max_bananas max_iterations acc i =
  if max_iterations <= 0 then 
    acc
  else
    let j = next i in
    let k = next j in
    let l = next k in
    let m = next l in
    let a = j mod 10 - i mod 10 in
    let b = k mod 10 - j mod 10 in
    let c = l mod 10 - k mod 10 in
    let d = m mod 10 - l mod 10 in
    let key = String.concat ", " [string_of_int a; string_of_int b; string_of_int c; string_of_int d] in 
    find_max_bananas (max_iterations - 1) (StringMap.add key (last_digit m) acc) j

let secret_numbers =
  let read_file filename =
    let lines = ref [] in
    let chan = open_in filename in
    try
      while true; do
        lines := int_of_string (input_line chan) :: !lines
      done; !lines
    with End_of_file ->
      close_in chan;
      List.rev !lines ;;
  in
  let filename =
    match Sys.getenv_opt "AOC_TEST" with
    | Some _ -> "22.txt.test"
    | None -> "22.txt"
  in
  read_file filename

let part1 numbers =
  numbers
    |> List.map (secret_number 2000)
    |> List.fold_left (+) 0

let part2 numbers =
  let bananas =
    numbers
      |> List.map (find_max_bananas 2000 StringMap.empty)
      |> List.fold_left (StringMap.union (fun k a_v m_v -> Some (a_v + m_v))) StringMap.empty
  in StringMap.fold (fun _ a b -> max a b) bananas 0 

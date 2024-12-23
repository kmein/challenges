let next step0 =
  let prune x = x mod 16777216 (* 2**24 *); in
  let step1 = prune (Int.logxor step0 (Int.shift_left step0 6)); in
  let step2 = prune (Int.logxor step1 (Int.shift_right step1 5)); in
  let step3 = prune (Int.logxor step2 (Int.shift_left step2 11)); in
  step3

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

let rec secret_number step start =
  if step <= 0 then
    start
  else
    secret_number (step - 1) (next start)

let () =
  let filename =
    match Sys.getenv_opt "AOC_TEST" with
    | Some _ -> "22.txt.test"
    | None -> "22.txt"
  in
  let numbers = read_file filename in
  numbers
    |> List.map (secret_number 2000)
    |> List.fold_left (+) 0
    |> Printf.printf "%d\n"

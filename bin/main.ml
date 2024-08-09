let ( << ) f g x = g (f x)
let string_of_char c : string = String.make 1 c

let rec int_range ~s ~e =
  match s - e with 0 -> [] | _ -> s :: int_range ~s:(s + 1) ~e

let possible_lens = int_range ~s:1 ~e:11

type pos =
  | Programme
  | Definition
  | Definitions
  | Alpha
  | Alphanum
  | Lowercase
  | Uppercase
  | Digit
  | Identifier
  | Number
  | Objective

type temp =
  | RMany : temp list -> temp
  | RSome : temp list -> temp
  | G : pos list -> temp (* as in "Generator" *)
  | S : temp list -> temp (* as in "Sequence" *)
  | T : string list -> temp (* as in "Terminal" *)

let uppercase_chars =
  int_range ~s:65 ~e:91 |> List.map (char_of_int << string_of_char)

let lowercase_chars =
  int_range ~s:97 ~e:123 |> List.map (char_of_int << string_of_char)

let digits = int_range ~s:0 ~e:10 |> List.map string_of_int

(** This is likley a place for optimisation in the future as this is the core
    of our programme and we are essentially random accessing an array. There
    are definitely specific datastructures purpose built for doing this. *)
let sample_random l = List.nth l (Random.int (List.length l))

let hm =
  let open Hashtbl in
  let hm = create 12345 in
  add hm Lowercase [ [ T lowercase_chars ] ];
  add hm Uppercase [ [ T uppercase_chars ] ];
  add hm Alpha [ [ G [ Lowercase ] ]; [ G [ Uppercase ] ] ];
  add hm Alphanum [ [ G [ Alpha ] ]; [ G [ Digit ] ] ];
  add hm Digit [ [ T digits ] ];
  add hm Identifier [ [ G [ Alpha ]; RMany [ G [ Alphanum ] ] ] ];
  add hm Number [ [ RSome [ G [ Digit ] ] ] ];
  add hm Definition [ [ G [ Identifier ]; T [ "=" ]; G [ Number ] ] ];
  add hm Definitions [ [ RMany [ S [ G [ Definition ]; T [ ";" ] ] ] ] ];
  add hm Objective [ [ T [ "min"; "max" ]; T [ ":" ]; G [ Number ] ] ];
  add hm Programme [ [ G [ Definitions ]; G [ Objective ]; T [ ";" ] ] ];
  hm

let rec idk_yet (part_of_sp : pos) =
  let (pos_parts : temp list list) = Hashtbl.find hm part_of_sp in
  let rec acc_f acc b =
    let rec loop rule reps =
      match reps with
      | 0 -> ""
      | x when x < 0 -> assert false
      | _ -> acc_f "" (rule |> sample_random) ^ loop rule (reps - 1)
    in
    acc
    ^
    match b with
    | RMany rule ->
        let reps = sample_random (0 :: possible_lens) in
        loop rule reps
    | RSome rule ->
        let reps = sample_random possible_lens in
        loop rule reps
    | G pos -> idk_yet (pos |> sample_random)
    | T term -> term |> sample_random
    | S terms -> List.fold_left acc_f "" terms
  in
  List.fold_left acc_f "" (sample_random pos_parts)

let () =
  Random.self_init ();
  idk_yet Programme |> print_endline

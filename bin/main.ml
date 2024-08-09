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
  | AExp
  | MExp
  | Term
  | EnumLit
  | Enum

let _show_pos = function
  | Programme -> "Programme"
  | Definition -> "Definition"
  | Definitions -> "Definitions"
  | Alpha -> "Alpha"
  | Alphanum -> "Alphanum"
  | Lowercase -> "Lowercase"
  | Uppercase -> "Uppercase"
  | Digit -> "Digit"
  | Identifier -> "Identifier"
  | Number -> "Number"
  | Objective -> "Objective"
  | AExp -> "AExp"
  | MExp -> "MExp"
  | Term -> "Term"
  | EnumLit -> "EnumLit"
  | Enum -> "Enum"

type temp =
  | RMany : temp list -> temp
  | RSome : temp list -> temp
  | G : pos list -> temp (* as in "Generator" *)
  | S : temp list -> temp (* as in "Sequence" *)
  | T : string list -> temp (* as in "Terminal" *)
  | O : temp -> temp

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
  add hm AExp [ [ G [ MExp ] ]; [ G [ MExp ]; T [ " + "; " - " ]; G [ AExp ] ] ];
  add hm MExp [ [ G [ Term ] ]; [ G [ Term ]; T [ " * "; " / " ]; G [ MExp ] ] ];
  add hm EnumLit
    [
      [ T [ "{ " ]; RSome [ G [ Alphanum ] ]; O (T [ "," ]); T [ " }" ] ];
      [
        T [ "{ " ];
        S
          [
            RSome [ G [ Alphanum ] ];
            RSome [ S [ T [ ", " ]; RSome [ G [ Alphanum ] ] ] ];
            O (T [ "," ]);
          ];
        T [ " }" ];
      ];
    ];
  add hm Enum [ [ G [ Identifier ] ]; [ G [ EnumLit ] ] ];
  add hm Term
    [
      [ G [ Number ] ];
      [ G [ Identifier ] ];
      [ G [ Number ] ];
      [ G [ Identifier ] ];
      [ S [ T [ "(" ]; G [ AExp ]; T [ ")" ] ] ];
    ];
  add hm Definition [ [ G [ Identifier ]; T [ " = " ]; G [ AExp; Enum ] ] ];
  add hm Definitions [ [ RMany [ S [ G [ Definition ]; T [ ";\n" ] ] ] ] ];
  add hm Objective [ [ T [ "min"; "max" ]; T [ ": " ]; G [ AExp ] ] ];
  add hm Programme [ [ G [ Definitions ]; G [ Objective ]; T [ ";" ] ] ];
  hm

let get_all_nested_pos r =
  let rec get_all_nested_pos' r =
    match r with
    | [] -> []
    | RMany rule :: rest -> get_all_nested_pos' rule @ get_all_nested_pos' rest
    | RSome rule :: rest -> get_all_nested_pos' rule @ get_all_nested_pos' rest
    | S rules :: rest -> get_all_nested_pos' rules @ get_all_nested_pos' rest
    | T _ :: rest -> get_all_nested_pos' rest
    | G pos :: rest -> pos :: get_all_nested_pos' rest
    | O rule :: rest -> get_all_nested_pos' [ rule ] @ get_all_nested_pos' rest
  in
  get_all_nested_pos' (List.concat r) |> List.concat

let _is_recursive pos =
  let rec loop pos (seen : 'a list) =
    let rule = Hashtbl.find hm pos in
    let poss = get_all_nested_pos rule in
    if List.exists (fun p -> List.exists (( = ) p) seen) poss then true
    else List.exists (fun a -> loop a (pos :: seen)) poss
  in
  loop pos []

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
    | O rule -> if Random.int 101 >= 50 then acc_f "" rule else ""
  in
  List.fold_left acc_f "" (sample_random pos_parts)

let () =
  Random.self_init ();
  idk_yet Programme |> print_endline

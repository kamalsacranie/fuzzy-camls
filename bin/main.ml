let ( << ) f g x = g (f x)
let string_of_char c : string = String.make 1 c

(* If we ever rewrite:
   - Make every part of speech have some sort of importance rating and
     repetition rules
   - Maybe call them something else than parts of speech...
   - Make different gramamrs composable
   - Make every thing just take one "temp" type. Right now, we have some
     instances where we have to declare an explicit sequence and others where a
     list implies a sequence, this is bad design
   - There's just too many ways to have optional things
   - Add some way to denote that we can surround some rule with whitespace
*)

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
  | DAExp
  | DMExp
  | DTerm
  | EnumLit
  | Enum
  | Array
  | Unary
  | MAExp
  | MMExp
  | MTerm
  | MIdentifier
  | Sum
  | Constraint
  | Constraints

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
  | DAExp -> "DAExp"
  | DMExp -> "DMExp"
  | DTerm -> "DTerm"
  | EnumLit -> "EnumLit"
  | Enum -> "Enum"
  | Array -> "Array"
  | Unary -> "Unary"
  | MAExp -> "MAExp"
  | MMExp -> "MMExp"
  | MTerm -> "MTerm"
  | MIdentifier -> "MIdentifier"
  | Sum -> "Sum"
  | Constraint -> "Constraint"
  | Constraints -> "Constraints"

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
  let alnum_str = RSome [ G [ Alphanum ] ] in
  let sep_pos_end_opt pos sep = S [ pos; RMany [ S [ sep; pos ] ]; O sep ] in
  let paren_exp inner = [ S [ T [ "(" ]; G [ inner ]; T [ ")" ] ] ] in
  let list_like lbend pos sep rbend =
    [ lbend; sep_pos_end_opt pos sep; rbend ]
  in
  let hm = create 12345 in
  add hm Lowercase [ [ T lowercase_chars ] ];
  add hm Uppercase [ [ T uppercase_chars ] ];
  add hm Alpha [ [ G [ Lowercase ] ]; [ G [ Uppercase ] ] ];
  add hm Alphanum [ [ G [ Alpha ] ]; [ G [ Digit ] ] ];
  add hm Digit [ [ T digits ] ];
  add hm Identifier [ [ G [ Alpha ]; RMany [ G [ Alphanum ] ] ] ];
  add hm MIdentifier
    [
      [ G [ Identifier ] ];
      [ G [ Uppercase ]; RMany [ S [ T [ "_" ]; G [ Lowercase ] ] ] ];
    ];
  add hm Number [ [ RSome [ G [ Digit ] ] ] ];
  add hm Array
    [
      list_like (T [ "[ " ])
        (G [ Array; DAExp; DAExp; DAExp; DAExp; DAExp; DAExp; DAExp ])
        (T [ ", " ]) (T [ " ]" ]);
    ];
  add hm DAExp
    [ [ G [ DMExp ] ]; [ G [ DMExp ]; T [ " + "; " - " ]; G [ DAExp ] ] ];
  add hm DMExp
    [ [ G [ DTerm ] ]; [ G [ DTerm ]; T [ " * "; " / " ]; G [ DMExp ] ] ];
  add hm MAExp
    [ [ G [ MMExp ] ]; [ G [ MMExp ]; T [ " + "; " - " ]; G [ MAExp ] ] ];
  add hm MMExp
    [ [ G [ MTerm ] ]; [ G [ MTerm ]; T [ " * "; " / " ]; G [ MMExp ] ] ];
  add hm EnumLit [ list_like (T [ "{ " ]) alnum_str (T [ ", " ]) (T [ " }" ]) ];
  add hm Enum [ [ G [ Identifier ] ]; [ G [ EnumLit ] ] ];
  add hm Unary [ [ T [ "-"; "+" ]; G [ DAExp ] ] ];
  add hm Sum
    [
      [
        T [ "SUM" ];
        T [ "{" ];
        S [ G [ Lowercase ]; T [ " = " ]; G [ Enum ] ];
        T [ "}" ];
      ];
    ];
  add hm MTerm
    [ [ G [ Sum ] ]; [ G [ MIdentifier ] ]; [ G [ Number ] ]; paren_exp MAExp ];
  add hm DTerm
    [ [ G [ Number ] ]; [ G [ Unary ] ]; [ G [ Identifier ] ]; paren_exp DAExp ];
  add hm Definition
    [
      [
        G [ Identifier ];
        T [ " = " ];
        G [ DAExp; Enum; Array; DAExp; Enum; DAExp ];
      ];
    ];
  add hm Constraint
    [
      [
        G [ MAExp ];
        T ([ "<="; ">="; "=="; ">"; "<" ] |> List.map (fun s -> " " ^ s ^ " "));
        G [ MAExp ];
      ];
    ];
  add hm Constraints [ [ RMany [ S [ G [ Constraint ]; T [ ";\n" ] ] ] ] ];
  add hm Definitions [ [ RMany [ S [ G [ Definition ]; T [ ";\n" ] ] ] ] ];
  add hm Objective [ [ T [ "min"; "max" ]; T [ ": " ]; G [ MAExp ] ] ];
  add hm Programme
    [ [ G [ Definitions ]; G [ Objective ]; T [ ";\n" ]; G [ Constraints ] ] ];
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
  let te = ref (sample_random pos_parts) in
  for _ = 0 to 1 do
    te :=
      if List.exists _is_recursive (get_all_nested_pos [ !te ]) then
        sample_random pos_parts
      else !te
  done;
  List.fold_left acc_f "" !te

let () =
  Random.self_init ();
  idk_yet Programme |> print_endline

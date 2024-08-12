let ( << ) f g x = g (f x)
let string_of_char c : string = String.make 1 c
let flip f x y = f y x

type part_of_sp =
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
  | MUnary
  | DUnary
  | MAExp
  | MMExp
  | MTerm
  | MIdentifier
  | Sum
  | Constraint
  | Constraints

type clause =
  | Many : clause -> clause
  | G : part_of_sp -> clause (* as in "Generator" *)
  | S : clause list -> clause (* as in "Sequence" *)
  | T : string -> clause (* as in "Terminal" *)
  | O : clause -> clause
  | C : clause list -> clause

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
   - Maybe make it so that multiple keys can yield the same rule from the hash
     map
*)

let rec int_range ~s ~e =
  match s - e with 0 -> [] | _ -> s :: int_range ~s:(s + 1) ~e

(** This is likley a place for optimisation in the future as this is the core
    of our programme and we are essentially random accessing an array. There
    are definitely specific datastructures purpose built for doing this. *)
let sample_random l = List.nth l (Random.int (List.length l))

let sample_bool = sample_random [ true; false ]
let possible_lens = int_range ~s:0 ~e:11
let ( => ) r e x = Hashtbl.add x r e
let ( <|> ) rs r = match rs with C rs -> C (rs @ [ r ]) | _ -> C [ rs; r ]
let ( <&> ) rs r = match rs with S rs -> S (rs @ [ r ]) | _ -> S [ rs; r ]
let ( !<.> ) r = G r
let ( ?> ) r = O r
let many r = Many r
let some r = r <&> Many r

let rec rept rule reps =
  match reps with
  | 0 -> rule
  | x when x < 0 -> failwith "can't repeat a rule negative times"
  | _ -> rule <&> rept rule (reps - 1)

let ( !> ) r = T r

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
  | MUnary -> "MUnary"
  | DUnary -> "DUnary"
  | MAExp -> "MAExp"
  | MMExp -> "MMExp"
  | MTerm -> "MTerm"
  | MIdentifier -> "MIdentifier"
  | Sum -> "Sum"
  | Constraint -> "Constraint"
  | Constraints -> "Constraints"

let uppercase_chars =
  int_range ~s:65 ~e:91 |> List.map (char_of_int << string_of_char)

let lowercase_chars =
  int_range ~s:97 ~e:123 |> List.map (char_of_int << string_of_char)

let digits = int_range ~s:0 ~e:10 |> List.map string_of_int
let non_zero_digits = int_range ~s:1 ~e:10 |> List.map string_of_int

let hm =
  let open List in
  let sep_pos_end_opt pos sep =
    pos <&> many (sep <&> !>" " <&> pos) <&> ?>sep
  in
  let paren_exp inner = !>"(" <&> inner <&> !>")" in
  let list_like lbend pos sep rbend =
    lbend <&> sep_pos_end_opt pos sep <&> rbend
  in
  let hm = Hashtbl.create 12345 in
  let choice_of_string_lits ss = ss |> map ( !> ) |> fold_left ( <|> ) (C []) in

  [
    Uppercase => (uppercase_chars |> choice_of_string_lits);
    Lowercase => (lowercase_chars |> choice_of_string_lits);
    Alpha => (!<.>Lowercase <|> !<.>Uppercase);
    Digit => (digits |> choice_of_string_lits);
    Alphanum => (!<.>Alpha <|> !<.>Digit);
    Number
    => (non_zero_digits |> choice_of_string_lits
       <&> many (!<.>Digit <|> (!>"_" <&> rept !<.>Digit 3)));
    Identifier => (!<.>Alpha <&> some !<.>Alphanum);
    MIdentifier
    => (!<.>Identifier
       <|> (?>(!>"~" <|> !>"`")
           <&> !<.>Uppercase
           <&> (!>"_" <&> !<.>Lowercase |> many)));
    Array => list_like !>"[ " (!<.>Array <|> !<.>DAExp) !>"," !>" ]";
    DAExp => (!<.>DMExp <|> (!<.>DMExp <&> (!>" + " <|> !>" - ") <&> !<.>DAExp));
    DMExp => (!<.>DTerm <|> (!<.>DTerm <&> (!>" * " <|> !>" / ") <&> !<.>DMExp));
    DTerm
    => (!<.>Number <|> !<.>Identifier <|> paren_exp !<.>DAExp <|> !<.>DUnary);
    MAExp => (!<.>MMExp <|> (!<.>MMExp <&> (!>" + " <|> !>" - ") <&> !<.>MAExp));
    MMExp => (!<.>MTerm <|> (!<.>MTerm <&> (!>" * " <|> !>" / ") <&> !<.>MMExp));
    MTerm => (!<.>Number <|> !<.>MIdentifier <|> paren_exp !<.>MAExp <|> !<.>Sum);
    EnumLit => list_like !>"{ " (some !<.>Alphanum) !>"," !>" }";
    (* TODO: Enum filter *)
    Enum => (!<.>Identifier <|> !<.>EnumLit);
    DUnary => (!>"+" <|> !>"-" <&> !<.>DTerm);
    MUnary => (!>"+" <|> !>"-" <&> !<.>MTerm);
    Sum
    => (!>"SUM" <&> !>"{" <&> !<.>Lowercase <&> !>" = " <&> !<.>Enum <&> !>"}"
       <&> (!<.>MMExp <|> paren_exp !<.>MAExp));
    Definition
    => (!<.>Identifier <&> !>" = "
       <&> (!<.>DAExp <|> !<.>Enum (*  <|> !<.>Array *)));
    Constraint
    => (!<.>MAExp
       <&> ([ "<="; ">="; "=="; ">"; "<" ]
           |> List.map (fun s -> " " ^ s ^ " ")
           |> choice_of_string_lits)
       <&> !<.>MAExp);
    Constraints => many (!<.>Constraint <&> !>";\n");
    Definitions => many (!<.>Definition <&> !>";\n");
    Objective
    => ([ "min"; "max" ] |> choice_of_string_lits <&> !>": " <&> !<.>MAExp);
    Programme
    => (!<.>Definitions <&> !<.>Objective <&> !>";\n" <&> !<.>Constraints);
  ]
  |> List.iter (fun f -> f hm);
  hm

let min_path_score (clause : clause) =
  let rec shortest' deep seen clause =
    let s = shortest' (deep + 1) in
    match clause with
    | Many rule -> s seen rule
    | G pos ->
        if List.exists (( = ) pos) seen then Int.max_int
        else s (pos :: seen) (Hashtbl.find hm pos)
    | T _ -> deep
    | O rule -> s seen rule
    | S rules ->
        let r =
          rules |> List.map (s seen) |> List.fold_left Int.max Int.min_int
        in
        if r = Int.min_int then Int.max_int else r
    | C rules ->
        rules |> List.map (s seen) |> List.fold_left Int.min Int.max_int
  in
  shortest' 0 [] clause

(** Randomly selects the shortest scoring rule path for a given rule
  *)
let select_shortest rules =
  let sorted_rules =
    rules |> List.map min_path_score |> List.combine rules
    |> List.sort (fun (_, a) (_, b) -> a - b)
  in
  let best_score = List.hd sorted_rules |> snd in
  List.filter (snd << ( == ) best_score) sorted_rules |> sample_random |> fst

let rec get_all_nested_pos rule seen =
  match rule with
  | Many rule -> get_all_nested_pos rule seen
  | S rules -> List.concat (List.map (fun r -> get_all_nested_pos r seen) rules)
  | T _ -> []
  | G pos ->
      pos
      ::
      (if List.exists (( = ) pos) seen then []
       else get_all_nested_pos (Hashtbl.find hm pos) (pos :: seen))
  | O rule -> get_all_nested_pos rule seen
  | C rules -> List.concat (List.map (fun r -> get_all_nested_pos r seen) rules)

let _is_recursive part_of_sp =
  let rec loop pos (seen : 'a list) =
    let rule = Hashtbl.find hm pos in
    let poss = get_all_nested_pos rule [ pos ] in
    List.exists
      (fun p -> List.exists (( = ) p) seen)
      poss (* better to use a hashmap because of this step *)
    || List.exists (fun p -> loop p (pos :: seen)) poss
  in
  match part_of_sp with G pos -> loop pos [ pos ] | _ -> false

let rec generate_source' (clause : clause) (depth, returning) =
  let rec loop rule reps =
    match reps with
    | 0 -> ""
    | x when x < 0 -> failwith "can't repeat a rule negative times"
    | _ -> generate_source' rule (depth + 1, returning) ^ loop rule (reps - 1)
  in

  match clause with
  | Many rule -> loop rule (sample_random possible_lens)
  | G pos -> generate_source' (Hashtbl.find hm pos) (depth + 1, returning)
  | T string_lit -> string_lit
  | S rules ->
      rules
      |> List.map ((flip generate_source') (depth + 1, returning))
      |> List.fold_left ( ^ ) ""
  | O rule ->
      if sample_bool then generate_source' rule (depth + 1, returning) else ""
  | C cs ->
      generate_source'
        (if depth > 70 || returning then select_shortest cs
         else sample_random cs)
        (depth + 1, depth > 70 || returning)

let generate_source pos = generate_source' (G pos) (0, false)

let () =
  Random.self_init ();
  generate_source Programme |> print_endline

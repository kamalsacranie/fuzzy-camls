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
  | TRMany : clause -> clause
  | TG : part_of_sp -> clause (* as in "Generator" *)
  | TS : clause list -> clause (* as in "Sequence" *)
  | TT : string -> clause (* as in "Terminal" *)
  | TO : clause -> clause
  | TC : clause list -> clause

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

let possible_lens = int_range ~s:0 ~e:10
let ( => ) r e x = Hashtbl.add x r e
let ( <|> ) rs r = match rs with TC rs -> TC (rs @ [ r ]) | _ -> TC [ rs; r ]
let ( <&> ) rs r = match rs with TS rs -> TS (rs @ [ r ]) | _ -> TS [ rs; r ]
let ( !<.> ) r = TG r
let ( ?> ) r = TO r
let many r = TRMany r
let some r = r <&> TRMany r

let rec rept rule reps =
  match reps with
  | 0 -> rule
  | x when x < 0 -> failwith "can't repeat a rule negative times"
  | _ -> rule <&> rept rule (reps - 1)

let ( !> ) r = TT r

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

(** This is likley a place for optimisation in the future as this is the core
    of our programme and we are essentially random accessing an array. There
    are definitely specific datastructures purpose built for doing this. *)
let sample_random l = List.nth l (Random.int (List.length l))

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
  let choice_of_string_lits ss =
    ss |> map ( !> ) |> fold_left ( <|> ) (TC [])
  in

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
    Array => list_like !>"[ " (!<.>Array <|> !<.>DAExp) !>"," !>"[ ";
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
    Definition => (!<.>Identifier <&> !>" = " <&> !<.>DAExp);
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

let _shortest (clause : clause) =
  let rec shortest' len seen clause =
    let s = shortest' (len + 1) in
    match clause with
    | TRMany rule -> s seen rule
    | TG pos ->
        if List.exists (( = ) pos) seen then Int.max_int
        else s (pos :: seen) (Hashtbl.find hm pos)
    | TT _ -> len
    | TO rule -> s seen rule
    | TS rules ->
        let r =
          rules |> List.map (s seen) |> List.fold_left Int.max Int.min_int
        in
        if r = Int.min_int then Int.max_int else r
    | TC rules ->
        rules |> List.map (s seen) |> List.fold_left Int.min Int.max_int
  in
  shortest' 0 [] clause

let select_shortest rules =
  rules |> List.map _shortest |> List.combine rules
  |> List.sort (fun (_, a) (_, b) -> a - b)
  |> List.hd |> fst

let rec get_all_nested_pos rule seen =
  match rule with
  | TRMany rule -> get_all_nested_pos rule seen
  | TS rules ->
      List.concat (List.map (fun r -> get_all_nested_pos r seen) rules)
  | TT _ -> []
  | TG pos ->
      pos
      ::
      (if List.exists (( = ) pos) seen then []
       else get_all_nested_pos (Hashtbl.find hm pos) (pos :: seen))
  | TO rule -> get_all_nested_pos rule seen
  | TC rules ->
      List.concat (List.map (fun r -> get_all_nested_pos r seen) rules)

let _is_recursive part_of_sp =
  let rec loop pos (seen : 'a list) =
    let rule = Hashtbl.find hm pos in
    let poss = get_all_nested_pos rule [ pos ] in
    List.exists
      (fun p -> List.exists (( = ) p) seen)
      poss (* better to use a hashmap because of this step *)
    || List.exists (fun p -> loop p (pos :: seen)) poss
  in
  match part_of_sp with TG pos -> loop pos [ pos ] | _ -> false

let rec generate_source' (clause : clause) depth =
  let rec loop rule reps =
    match reps with
    | 0 -> ""
    | x when x < 0 -> failwith "can't repeat a rule negative times"
    | _ -> generate_source' rule (depth + 1) ^ loop rule (reps - 1)
  in

  let clause =
    if depth > 20 && _is_recursive clause then !<.>Number else clause
  in

  match clause with
  | TRMany rule -> loop rule (sample_random possible_lens)
  | TG pos -> generate_source' (Hashtbl.find hm pos) (depth + 1)
  | TT string_lit -> string_lit
  | TS rules ->
      rules
      |> List.map ((flip generate_source') (depth + 1))
      |> List.fold_left ( ^ ) ""
  | TO rule ->
      if Random.int 101 >= 50 then generate_source' rule (depth + 1) else ""
  | TC cs ->
      generate_source'
        (if depth > 1_000 then select_shortest cs else sample_random cs)
        (depth + 1)

let generate_source pos = generate_source' (TG pos) 0

let () =
  Random.self_init ();
  generate_source Programme |> print_endline

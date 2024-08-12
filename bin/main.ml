open Programme_generator.Utils
open Programme_generator.Generator

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

let possible_lens = int_range ~s:0 ~e:5

let uppercase_chars =
  int_range ~s:65 ~e:91 |> List.map (char_of_int << string_of_char)

let lowercase_chars =
  int_range ~s:97 ~e:123 |> List.map (char_of_int << string_of_char)

let digits = int_range ~s:0 ~e:10 |> List.map string_of_int
let non_zero_digits = int_range ~s:1 ~e:10 |> List.map string_of_int

let _kill_grammar =
  let open List in
  let sep_pos_end_opt pos sep =
    pos <&> many (sep <&> !>" " <&> pos) <&> ?>sep
  in
  let paren_exp inner = !>"(" <&> inner <&> !>")" in
  let list_like lbend pos sep rbend =
    lbend <&> sep_pos_end_opt pos sep <&> rbend
  in
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
    => (!<.>Identifier <&> !>" = " <&> (!<.>DAExp <|> !<.>Enum <|> !<.>Array));
    Constraint
    => (!<.>MAExp
       <&> ([ "<="; ">="; "=="; ">"; "<" ]
           |> List.map (( ^ ) " " << ( ^ ) " ")
           |> choice_of_string_lits)
       <&> !<.>MAExp);
    Constraints => many (!<.>Constraint <&> !>";\n");
    Definitions => many (!<.>Definition <&> !>";\n");
    Objective
    => ([ "min"; "max" ] |> choice_of_string_lits <&> !>": " <&> !<.>MAExp);
    Programme
    => (!<.>Definitions <&> !<.>Objective <&> !>";\n" <&> !<.>Constraints);
  ]

let numeric_expression_grammar =
  let open List in
  let choice_of_string_lits ss = ss |> map ( !> ) |> fold_left ( <|> ) (C []) in

  [
    Digit => (digits |> choice_of_string_lits);
    Number
    => (non_zero_digits |> choice_of_string_lits
       <&> (many !<.>Digit <|> many (!>"_" <&> rept !<.>Digit 3)));
    DAExp => (!<.>DMExp <|> (!<.>DMExp <&> (!>" + " <|> !>" - " <&> !<.>DAExp)));
    DMExp => (!<.>DTerm <|> (!<.>DTerm <&> (!>" * " <|> !>" / ") <&> !<.>DMExp));
    DTerm => (!<.>Number <|> !<.>DAExp);
  ]

let () =
  generate_source DAExp numeric_expression_grammar possible_lens 100
  |> print_endline

type 'a clause =
  | Many : 'a clause -> 'a clause
  | G : 'a -> 'a clause (* as in "Generator" *)
  | S : 'a clause list -> 'a clause (* as in "Sequence" *)
  | T : string -> 'a clause (* as in "Terminal" *)
  | O : 'a clause -> 'a clause
  | C : 'a clause list -> 'a clause

let ( => ) r e x = Hashtbl.add x r e
let ( <|> ) rs r = match rs with C rs -> C (rs @ [ r ]) | _ -> C [ rs; r ]
let ( <&> ) rs r = match rs with S rs -> S (rs @ [ r ]) | _ -> S [ rs; r ]
let ( !<.> ) r = G r
let ( ?> ) r = O r
let ( !> ) r = T r
let many r = Many r
let some r = r <&> Many r

let rec rept rule reps =
  match reps with
  | 1 -> rule
  | x when x <= 0 -> failwith "can't repeat a rule zero or less times"
  | _ -> rule <&> rept rule (reps - 1)

let min_path_score hm (clause : 'a clause) =
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
let select_shortest hm rules =
  let open Utils in
  let sorted_rules =
    rules
    |> List.map (min_path_score hm)
    |> List.combine rules
    |> List.sort (fun (_, a) (_, b) -> a - b)
  in
  let best_score = List.hd sorted_rules |> snd in
  List.filter (snd << ( == ) best_score) sorted_rules |> sample_random |> fst

let rec generate_source' max_depth pos_lens hm (clause : 'a clause)
    (depth, returning) =
  let rec loop rule reps =
    match reps with
    | 0 -> ""
    | x when x < 0 -> failwith "can't repeat a rule negative times"
    | _ ->
        generate_source' max_depth pos_lens hm rule (depth + 1, returning)
        ^ loop rule (reps - 1)
  in

  let open Utils in
  match clause with
  | Many rule -> loop rule (sample_random pos_lens)
  | G pos ->
      generate_source' max_depth pos_lens hm (Hashtbl.find hm pos)
        (depth + 1, returning)
  | T string_lit -> string_lit
  | S rules ->
      rules
      |> List.map
           ((flip (generate_source' max_depth pos_lens hm))
              (depth + 1, returning))
      |> List.fold_left ( ^ ) ""
  | O rule ->
      if sample_bool then
        generate_source' max_depth pos_lens hm rule (depth + 1, returning)
      else ""
  | C cs ->
      let returning = depth > max_depth || returning in
      generate_source' max_depth pos_lens hm
        (if returning then select_shortest hm cs else sample_random cs)
        (depth + 1, returning)

let generate_source pos rules pos_lens max_depth =
  Random.self_init ();
  let hm = Hashtbl.create 12345 in
  rules |> List.iter (fun f -> f hm);
  generate_source' max_depth pos_lens hm (G pos) (0, false)

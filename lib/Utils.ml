let ( << ) f g x = g (f x)
let string_of_char c : string = String.make 1 c
let flip f x y = f y x

let rec int_range ~s ~e =
  match s - e with 0 -> [] | _ -> s :: int_range ~s:(s + 1) ~e

(** This is likley a place for optimisation in the future as this is the core
    of our programme and we are essentially random accessing an array. There
    are definitely specific datastructures purpose built for doing this. *)
let sample_random l = List.nth l (Random.int (List.length l))

let sample_bool = sample_random [ true; false ]

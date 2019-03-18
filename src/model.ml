type t = {
  vec : float array array;
  dict : (string, int) Hashtbl.t;
  common_words : string array;
}

let euclidian_distance a b =
  let dim = Array.length a in
  let d = ref 0. in
  for i = 0 to dim - 1 do
    d := !d +. (a.(i) -. b.(i)) ** 2.
  done;
  sqrt !d

let norm v =
  let dim = Array.length v in
  let n = ref 0. in
  for i = 0 to dim - 1 do
    n := !n +. v.(i) ** 2.
  done;
  sqrt !n

let angular_distance a b =
  let dim = Array.length a in
  let c = ref 0. in
  for i = 0 to dim - 1 do
    c := !c +. a.(i) *. b.(i)
  done;
  let c = min 1. (!c /. norm a /. norm b) in
  acos c

let distance = angular_distance
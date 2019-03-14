type word = {
  str : string;
  vec : float array;
}

type t = {
  words : word array;
  dict : (string, int) Hashtbl.t;
}
open Model

let max_value = 6.

let float_of_fixed a b =
  let n = a + b lsl 8 in
  let n =
    if n < (1 lsl 15) then n
    else n - (1 lsl 16)
  in
  float n /. float (1 lsl 15) *. max_value

let load w d c a =
  let dict = Hashtbl.create w in
  let common_words = Array.make c "" in
  let vec = Array.make w [||] in
  let rec parse_string i b =
    if a.(i) = Char.code ' ' then
      Bytes.to_string (Buffer.to_bytes b), i + 1
    else begin
      Buffer.add_char b (Char.chr a.(i));
      parse_string (i + 1) b
    end
  in
  let i = ref 0 in
  for j = 0 to w - 1 do
    let b = Buffer.create 3 in
    let str, ii = parse_string !i b in
    Hashtbl.add dict str j;
    if j >= 50 && j < c + 50 then
      common_words.(j - 50) <- str;
    let v = Array.make d 0. in
    for k = 0 to d - 1 do
      v.(k) <- float_of_fixed a.(ii + 2 * k) a.(ii + 2 * k + 1)
    done;
    i := ii + 2 * d;
    vec.(j) <- v
  done;
  { vec; dict; common_words }

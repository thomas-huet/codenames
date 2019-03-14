open Model

let max_value = 6.

let float_of_fixed a b =
  let n = a + b lsl 8 in
  let n =
    if n < (1 lsl 15) then n
    else n - (1 lsl 16)
  in
  float n /. float (1 lsl 15) *. max_value

let parse dict w d a =
  let model = Array.make w [||] in
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
    let vec = Array.make d 0. in
    for k = 0 to d - 1 do
      vec.(k) <- float_of_fixed a.(ii + 2 * k) a.(ii + 2 * k + 1)
    done;
    i := ii + 2 * d;
    model.(j) <- vec
  done;
  model

let load w d a =
  let dict = Hashtbl.create w in
  let vec = parse dict w d a in
  { vec; dict }

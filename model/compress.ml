let convert_num s =
  let out = Bytes.create 3 in
  let neg = s.[0] = '-' in
  let rec scan_e i a =
    if i = String.length s then a else
    scan_e (i + 1) (10 * a + Char.code s.[i] - Char.code '0')
  in
  let rec scan i a d =
    if i = String.length s then a, d else
    match s.[i] with
    | '.' -> scan (i + 1) a 0
    | '0'..'9' as c -> scan (i + 1) (10 * a + Char.code c - Char.code '0') (d + 1)
    | 'e' -> a, d + scan_e (i + 2) 0
  in
  let n, e = scan (if neg then 1 else 0) 0 0 in
  assert (n < 1 lsl 17);
  assert (e < 1 lsl 6);
  Bytes.set out 0 (Char.chr (n land 255));
  Bytes.set out 1 (Char.chr ((n lsr 8) land 255));
  let byte = ((n lsr 16) land 1) lor ((e land 63) lsl 1) lor (if neg then 128 else 0) in
  Bytes.set out 2 (Char.chr byte);
  Bytes.to_string out

let rec main d input output =
  Scanf.bscanf input " %s" (fun s ->
    if s <> "" then begin
      Printf.fprintf output "%s " s;
      for _ = 0 to d - 1 do
        Scanf.bscanf input " %s" (fun n -> Printf.fprintf output "%s" (convert_num n))
      done;
      main d input output
    end)

let () =
  let d = ref 0 in
  let input = ref stdin in
  let set_input f =
    let ic = open_in f in
    input := ic
  in
  let output = ref stdout in
  let set_output f =
    let oc = open_out f in
    output := oc
  in
  let spec = [
    "-d", Arg.Int((:=) d), "Dimension of the vectors";
    "-o", Arg.String set_output, "Output file";
  ] in
  Arg.parse spec set_input "compress -d <dimension> [-o <output_file>] [<input_file>]";
  main !d (Scanf.Scanning.from_channel !input) !output;
  close_out !output;
  close_in !input

let fixed_of_float f =
  let n = int_of_float (f *. float (1 lsl 15) /. 6.) land 65535 in
  let b = Bytes.create 2 in
  Bytes.set b 0 (Char.chr (n land 255));
  Bytes.set b 1 (Char.chr (n lsr 8));
  Bytes.to_string b

let rec main d input output =
  Scanf.bscanf input " %s" (fun s ->
    if s <> "" then begin
      Printf.fprintf output "%s " s;
      for _ = 0 to d - 1 do
        Scanf.bscanf input " %f" (fun f -> Printf.fprintf output "%s" (fixed_of_float f))
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

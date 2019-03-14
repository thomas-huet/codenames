type 'a promise

type response

type array_buffer

external fetch : string -> response promise = "" [@@bs.val]

external continue : 'a promise -> ('a -> unit) -> unit = "then" [@@bs.send]

external array_buffer : response -> array_buffer promise = "arrayBuffer" [@@bs.send]

external create_byte_array : array_buffer -> int array = "Uint8Array" [@@bs.new]

let get url f =
  continue (fetch url)
    (fun response -> continue (array_buffer response)
      (fun a -> f (create_byte_array a)))
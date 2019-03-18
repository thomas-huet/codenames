open Model

type t = {
  word : string;
  n : int;
  score : float;
}

let eps = 0.001

let make model vectors word id =
  let u = model.vec.(id) in
  let distances = List.map (fun (v, color) -> distance u v, color) vectors in
  let distances = List.sort compare distances in
  let rec score n tot = function
  | [] -> invalid_arg "Not all cards should be good."
  | (d, `Green) :: t -> if d < eps then { word; n = 0; score = 0. } else score (n + 1) (tot +. d) t
  | (d, `White) :: _ -> { word; n; score = float n -. tot /. d }
  | (d, `Black) :: _ -> { word; n = n - 1; score = float (n - 1) -. tot /. d }
  in
  score 0 0. distances

let best model cards =
  let vec word = model.vec.(Hashtbl.find model.dict word) in
  let vectors = List.map (fun (w, color) -> vec w, color) cards in
  let best = ref { word = ""; n = 0; score = 0. } in
  let update_best word id =
    let hint = make model vectors word id in
    if hint.score > !best.score then
      best := hint
  in
  Hashtbl.iter update_best model.dict;
  (* Log *)
  let distances = List.sort compare (List.map (fun (w, _) -> (distance (vec !best.word) (vec w), w)) cards) in
  let () = List.iter (fun (d, w) -> Js.log (w ^ " " ^ string_of_float d)) distances in
  !best.word, !best.n
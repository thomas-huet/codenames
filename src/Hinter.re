type color = [`Green | `White | `Black];

let cycle = fun
| `Green => `White
| `White => `Black
| `Black => `Green;

type state = {
  cards : list((string, color)),
  hint : option((string, int)),
};

type action =
| Add_card(string)
| Change_color(string)
| Hint;

let component = ReasonReact.reducerComponent("Hinter");

let make = (~model, _children) => {
  ...component,
  initialState: () => {
    cards: [],
    hint: None,
  },
  reducer: (action, state) => switch (action) {
  | Add_card(word) => ReasonReact.Update({cards: [(word, `White), ...state.cards], hint: None})
  | Change_color(word) => {
    let cards = List.map(((w, color)) => if (w == word) {(w, cycle(color))} else {(w, color)}, state.cards);
    ReasonReact.Update({...state, cards: cards})
  }
  | Hint => ReasonReact.Update({...state, hint: Some(Hint.best(model, state.cards))})
  },
  render: ({state, send}) => {
    let cards = Array.of_list(List.map(((word, color)) => <Card classes=[color] onClick={Some(() => send(Change_color(word)))}>...word</Card>, state.cards));
    let button_disabled = List.for_all(((_, color)) => color == `Green, state.cards) || List.for_all(((_, color)) => color != `Green, state.cards);
    <>
      <div>
        <WordInput onSubmit={(word) => send(Add_card(word))} model=model button="Add word"/>
      </div>
      <button onClick={(_event) => send(Hint)} disabled=button_disabled>{ReasonReact.string("Get hint")}</button>
      {switch (state.hint) {
      | None => ReasonReact.null
      | Some((word, n)) => <div>{ReasonReact.string(word ++ " " ++ string_of_int(n))}</div>
      }}
      <div id="table">
        ...cards
      </div>
    </>
  }
}

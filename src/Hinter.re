type state = {
  green : list(string),
  white : list(string),
  black : list(string),
  hint : option((string, int)),
};

type action =
| Add_green(string)
| Add_white(string)
| Add_black(string)
| Hint;

let component = ReasonReact.reducerComponent("Hinter");

let make = (~model, _children) => {
  ...component,
  initialState: () => {
    green: [],
    white: [],
    black: [],
    hint: None,
  },
  reducer: (action, state) => switch (action) {
  | Add_green(word) => ReasonReact.Update({...state, green: [word, ...state.green], hint: None})
  | Add_white(word) => ReasonReact.Update({...state, white: [word, ...state.white], hint: None})
  | Add_black(word) => ReasonReact.Update({...state, black: [word, ...state.black], hint: None})
  | Hint =>
    if (state.green == []) {
      ReasonReact.NoUpdate
    } else {
      ReasonReact.Update({...state, hint: Some(Hint.best(model, state.green, state.white, state.black))})
    }
  },
  render: ({state, send}) => {
    let cards = Array.of_list(
      List.map((word) => <Card color=Card.Green>...word</Card>, state.green)
      @
      List.map((word) => <Card color=Card.White>...word</Card>, state.white)
      @
      List.map((word) => <Card color=Card.Black>...word</Card>, state.black)
    );
    <>
      <WordInput onSubmit={(word) => send(Add_green(word))} model=model>
        ...{ReasonReact.string("Add green word")}
      </WordInput>
      <WordInput onSubmit={(word) => send(Add_white(word))} model=model>
        ...{ReasonReact.string("Add white word")}
      </WordInput>
      <WordInput onSubmit={(word) => send(Add_black(word))} model=model>
        ...{ReasonReact.string("Add black word")}
      </WordInput>
      <button onClick={(_event) => send(Hint)}>{ReasonReact.string("Get hint")}</button>
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

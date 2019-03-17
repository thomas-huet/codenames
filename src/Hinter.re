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
    if (state.green == [] || (state.white == [] && state.black == [])) {
      ReasonReact.NoUpdate
    } else {
      ReasonReact.Update({...state, hint: Some(Hint.best(model, state.green, state.white, state.black))})
    }
  },
  render: ({state, send}) => {
    let cards = Array.of_list(
      List.map((word) => <Card classes=[`Green]>...word</Card>, state.green)
      @
      List.map((word) => <Card classes=[`White]>...word</Card>, state.white)
      @
      List.map((word) => <Card classes=[`Black]>...word</Card>, state.black)
    );
    <>
      <div>
        <WordInput onSubmit={(word) => send(Add_green(word))} model=model button="Add green word"/>
      </div>
      <div>
        <WordInput onSubmit={(word) => send(Add_white(word))} model=model button="Add white word"/>
      </div>
      <div>
        <WordInput onSubmit={(word) => send(Add_black(word))} model=model button="Add black word"/>
      </div>
      <button onClick={(_event) => send(Hint)} disabled={state.green == [] || (state.white == [] && state.black == [])}>{ReasonReact.string("Get hint")}</button>
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

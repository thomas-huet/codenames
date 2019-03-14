type state = {
  hint : option(string),
  words : list(string),
};

type action =
| Set_hint(option(string))
| Add_word(string)
| Remove_word(string);

let component = ReasonReact.reducerComponent("Guesser");

let make = (~model, _children) => {
  ...component,
  initialState: () => {
    hint: None,
    words: [],
  },
  reducer: (action, state) => switch action {
  | Set_hint(hint) => ReasonReact.Update({...state, hint})
  | Add_word(word) => ReasonReact.Update({...state, words: [word, ...state.words]})
  | Remove_word(word) => ReasonReact.Update({...state, words: List.filter((!=)(word), state.words)})
  },
  render: ({state, send}) => {
    let cards = Array.of_list(List.map((word) => <Card color=Card.White>...word</Card>, state.words));
    <>
      <WordSelector onChange={(word) => send(Set_hint(word))} model=model>
        ...{ReasonReact.string("Hint:")}
      </WordSelector>
      <WordSelector onSubmit={(word) => send(Add_word(word))} model=model>
        ...{ReasonReact.string("Add word")}
      </WordSelector>
      <div className="table">
        ...cards
      </div>
    </>
  }
}

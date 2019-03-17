open Model;

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
  reducer: (action, state) => switch (action) {
  | Set_hint(hint) => ReasonReact.Update({...state, hint})
  | Add_word(word) => ReasonReact.Update({...state, words: [word, ...state.words]})
  | Remove_word(word) => ReasonReact.Update({...state, words: List.filter((!=)(word), state.words)})
  },
  render: ({state, send}) => {
    let vec = (word) => model.vec[Hashtbl.find(model.dict, word)];
    let sorted_list = switch (state.hint) {
    | None => state.words
    | Some(word) => {
      let v = vec(word);
      let list = List.map((w) => (distance(v, vec(w)), w), state.words);
      List.map(snd, List.sort(compare, list))
    }
    };
    let cards = Array.of_list(List.map((word) => <Card classes=[`White]>...word</Card>, sorted_list));
    <>
      <div>
        {ReasonReact.string("Hint: ")}
        <WordInput onChange={(word) => send(Set_hint(word))} model=model/>
      </div>
      <div>
        <WordInput onSubmit={(word) => send(Add_word(word))} model=model button="Add word"/>
      </div>
      <div id="table">
        ...cards
      </div>
    </>
  }
}

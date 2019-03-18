type action = Word(option(string)) | Num(int);

let component = ReasonReact.reducerComponent("HintInput");

let make = (~onSubmit, ~model, _children) => {
  ...component,
  initialState: () => (None, 1),
  reducer: (action, (w, n)) => switch (action) {
  | Word(w) => ReasonReact.Update((w, n))
  | Num(n) => ReasonReact.Update((w, n))
  },
  render: ({send, state: (w, n)}) => {
    <div>
      <WordInput onSubmit={(word) => onSubmit(word, n)} onChange={(word) => send(Word(word))} model=model/>
      <input type_="number" min=1 max="9" value={string_of_int(n)} onChange={(event) => send(Num(int_of_string(ReactEvent.Form.target(event)##value)))}/>
      {switch (w) {
      | None => <button disabled=true>{ReasonReact.string("Give hint")}</button>
      | Some(word) => <button onClick={(_event) => onSubmit(word, n)}>{ReasonReact.string("Give hint")}</button>
      }}
    </div>
  }
}

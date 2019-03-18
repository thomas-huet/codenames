open Model;

let component = ReasonReact.reducerComponent("WordInput");

let make = (~onSubmit=?, ~onChange=ignore, ~model, ~button=?, _children) => {
  ...component,
  initialState: () => "",
  reducer: (action, _state) => ReasonReact.Update(action),
  render: ({send, state}) => {
    let valid = (w) => Hashtbl.mem(model.dict, w);
    let submit = switch (onSubmit) {
    | None => ignore
    | Some(submit) => () => if (valid(state)) {submit(state); send("")}
    };
    <>
      <input value=state onChange={(event) => {
        let e = ReactEvent.Form.target(event);
        let word = Js.String.toLowerCase(e##value);
        if (!valid(word)) {
          e##setCustomValidity("This word is not in the dictionary.");
          onChange(None)
        } else {
          e##setCustomValidity("");
          onChange(Some(word))
        }
        send(word)
      }} onKeyPress={(event) => {
        let key = ReactEvent.Keyboard.key(event);
        if (key == "Enter") {
          submit()
        }
      }}/>
      {switch (button) {
      | None => ReasonReact.null
      | Some(text) =>
        if (valid(state)) {
          <button onClick={(_event) => submit()}>{ReasonReact.string(text)}</button>
        } else {
          <button disabled=true>{ReasonReact.string(text)}</button>
        }
      }}
    </>
  }
}

open Model;

let component = ReasonReact.reducerComponent("WordSelector");

let make = (~onSubmit=?, ~onChange=ignore, ~model, child) => {
  ...component,
  initialState: () => "",
  reducer: (action, _state) => ReasonReact.Update(action),
  render: ({send, state}) => {
    let valid = (w) => Hashtbl.mem(model.dict, w);
    let submit = switch (onSubmit) {
    | None => ignore
    | Some(submit) => (_event) => if (valid(state)) {submit(state); send("")}
    };
    <div>
      child
      <input value=state onChange={(event) => {
        let e = ReactEvent.Form.target(event);
        let word = e##value;
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
      {switch (onSubmit) {
      | None => ReasonReact.null
      | Some(_) => <button onClick=submit>{ReasonReact.string("Submit")}</button>
      }}
    </div>
  }
}

type state =
| Loading
| Ready(Model.t);

let component = ReasonReact.reducerComponent("Starter");

let make = (_children) => {
  ...component,
  initialState: () => Loading,
  reducer: (model, _state) => ReasonReact.Update(Ready(model)),
  render: ({state, send}) => switch (state) {
  | Loading => {
      Fetch.get("model-100d-150k.bin", (data) => send(Loader.load(150_000, 100, data)));
      <div>{ReasonReact.string("Loading")}</div>
    }
  | Ready(model) =>
    <Main model=model/>
  }
}

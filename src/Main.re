type mode =
| Game
| Guesser
| Hinter;

let component = ReasonReact.reducerComponent("Main");

let make = (~model, _children) => {
  ...component,
  initialState: () => Game,
  reducer: (action, _state) => ReasonReact.Update(action),
  render: ({state, send}) =>
    <>
      <div>
        <button onClick={(_event) => send(Game)}>{ReasonReact.string("Game")}</button>
        <button onClick={(_event) => send(Guesser)}>{ReasonReact.string("Guesser")}</button>
        <button onClick={(_event) => send(Hinter)}>{ReasonReact.string("Hinter")}</button>
      </div>
      {switch (state) {
      | Game => <Game model=model/>
      | Guesser => <Guesser model=model/>
      | Hinter => <Hinter model=model/>
      }}
    </>
}

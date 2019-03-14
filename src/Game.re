let component = ReasonReact.statelessComponent("Game");

let make = (~model, _children) => {
  ...component,
  render: (_self) =>
    <>
      {ReasonReact.string("Game")}
    </>
}

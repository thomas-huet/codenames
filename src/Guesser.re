let component = ReasonReact.statelessComponent("Guesser");

let make = (~model, _children) => {
  ...component,
  render: (_self) =>
    <>
      {ReasonReact.string("Guesser")}
    </>
}

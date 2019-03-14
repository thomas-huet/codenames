let component = ReasonReact.statelessComponent("Hinter");

let make = (~model, _children) => {
  ...component,
  render: (_self) =>
    <>
      {ReasonReact.string("Hinter")}
    </>
}

open Model

let color_name = fun
| Green => "green"
| Black => "black"
| White => "white";

let component = ReasonReact.statelessComponent("Card");

let make = (~color, ~onClick=ignore, word) => {
  ...component,
  render: (_self) =>
    <div className={"card " ++ color_name(color)} onClick=onClick>
      {ReasonReact.string(word)}
    </div>
}

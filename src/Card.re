type color =
| Green
| Black
| White;

let color_name = fun
| Green => "green"
| Black => "black"
| White => "white";

let component = ReasonReact.statelessComponent("Card");

let make = (~color, word) => {
  ...component,
  render: (_self) =>
    <div className={"card " ++ color_name(color)}>
      {ReasonReact.string(word)}
    </div>
}

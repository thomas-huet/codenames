let class_name = fun
| `Green => "green"
| `White => "white"
| `Black => "black"
| `Me_wrong => "me_wrong"
| `They_wrong => "they_wrong"
| `Correct => "correct";

let component = ReasonReact.statelessComponent("Card");

let make = (~classes, ~onClick=None, word) => {
  ...component,
  render: (_self) => {
    let class_name = List.fold_left((name, c) => {name ++ " " ++ class_name(c)},"card", classes);
    let click = switch (onClick) {
    | None => ignore
    | Some(click) => (_event) => click();
    };
    <div className=class_name onClick=click>
      {ReasonReact.string(word)}
    </div>
  }
}

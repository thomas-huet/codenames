open Model;

Random.self_init();

type card_class = [`Green | `White | `Black | `Correct | `Me_wrong | `They_wrong];

type card = {
  word : string,
  my_color : [`Green | `White | `Black],
  their_color : [`Green | `White | `Black],
  guess : list([`Correct | `Me_wrong | `They_wrong]),
};

type state = {
  cards : array(card),
  hint : option((string, int)),
};

type action =
| Guess(int)
| Hint(string, int);

let component = ReasonReact.reducerComponent("Game");

let make = (~model, _children) => {
  ...component,
  initialState: () => {
    let rec random_word = () => {
      let word = model.common_words[Random.int(Array.length(model.common_words))];
      if (Js.Re.test(word, Js.Re.fromString("^[a-z]{3,}$"))) {
        word
      } else {
        random_word()
      }
    };
    let shuffle = (a) => {
      let n = Array.length(a);
      for (i in 0 to n - 1) {
        let tmp = a[i];
        let j = Random.int(n);
        a[i] = a[j];
        a[j] = tmp
      };
      a
    };
    let make_card = (my_color, their_color) => {
      word: random_word(),
      my_color: my_color,
      their_color: their_color,
      guess: [],
    };
    let cards = Array.init(25, (i) => {
      if (i == 0) {
        make_card(`Black, `Black)
      } else if (i == 1) {
        make_card(`Black, `Green)
      } else if (i == 2) {
        make_card(`Black, `White)
      } else if (i == 3) {
        make_card(`Green, `Black)
      } else if (i == 4) {
        make_card(`White, `Black)
      } else if (i < 8) {
        make_card(`Green, `Green)
      } else if (i < 13) {
        make_card(`Green, `White)
      } else if (i < 18) {
        make_card(`White, `Green)
      } else {
        make_card(`White, `White)
      }
    });
    let cards = shuffle(cards);
    { cards: cards, hint: None }
  },
  reducer: (action, state) => switch (action) {
  | Guess(i) => {
    switch (state.cards[i].their_color) {
    | `Green => {
      state.cards[i] = {...state.cards[i], guess: [`Correct]};
      ReasonReact.Update(state)
    }
    | `White => ReasonReact.NoUpdate // TODO
    | `Black => ReasonReact.NoUpdate // TODO
    }
  }
  | Hint(word, n) => ReasonReact.NoUpdate // TODO
  },
  render: ({state, send}) => {
    let cards = Array.mapi((i, card) => {
        let click = switch (state.hint) {
        | None => None
        | Some(_) =>
          if (List.mem(`Correct, card.guess) || List.mem(`Me_wrong, card.guess)) {
            None
          } else {
            Some(() => send(Guess(i)))
          }
        };
        <Card classes=[(card.my_color :> card_class), ...(card.guess :> list(card_class))] onClick=click>...card.word</Card>
      }, state.cards);
    <>
      {switch (state.hint) {
      | None => <HintInput onSubmit={(word, n) => send(Hint(word, n))} model=model/>
      | Some((word, n)) => ReasonReact.string(word ++ " " ++ string_of_int(n))
      }}
      <div id="table">
        ...cards
      </div>
    </>
  }
}

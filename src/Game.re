open Model;

Random.self_init();

type card = {
  word : string,
  my_color : color,
  their_color : color,
};

type state = {
  cards : array(card),
  hint : option(string),
};

type action =
| Guess(int)
| Hint(string);

let component = ReasonReact.reducerComponent("Game");

let make = (~model, _children) => {
  ...component,
  initialState: () => {
    let random_word = () => model.common_words[Random.int(Array.length(model.common_words))];
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
    };
    let cards = Array.init(25, (i) => {
      if (i == 0) {
        make_card(Black, Black)
      } else if (i == 1) {
        make_card(Black, Green)
      } else if (i == 2) {
        make_card(Black, White)
      } else if (i == 3) {
        make_card(Green, Black)
      } else if (i == 4) {
        make_card(White, Black)
      } else if (i < 8) {
        make_card(Green, Green)
      } else if (i < 13) {
        make_card(Green, White)
      } else if (i < 18) {
        make_card(White, Green)
      } else {
        make_card(White, White)
      }
    });
    let cards = shuffle(cards);
    { cards: cards, hint: None }
  },
  reducer: (action, state) => switch (action) {
  | Guess(i) => {
    switch (state.cards[i].their_color) {
    | Green => ReasonReact.NoUpdate
    | White => ReasonReact.NoUpdate
    | Black => ReasonReact.NoUpdate
    }
  }
  | Hint(word) => ReasonReact.NoUpdate
  },
  render: ({state, send}) => {
    let cards = Array.mapi((i, card) => <Card color=card.my_color onClick={(_event) => send(Guess(i))}>...card.word</Card>, state.cards);
    <>
      <div id="table">
        ...cards
      </div>
    </>
  }
}

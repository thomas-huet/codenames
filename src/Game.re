open Model;

Random.self_init();

[@bs.val] external alert : string => unit = "";

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
  turn : int,
};

type action =
| New
| Pass
| Guess(int)
| Hint(string, int);

let create = (model) => {
  let used_words = Hashtbl.create(25);
  let rec random_word = () => {
    let word = model.common_words[Random.int(Array.length(model.common_words))];
    if (Js.Re.test(word, Js.Re.fromString("^[a-z]{3,15}$")) && !Hashtbl.mem(used_words, word)) {
      Hashtbl.add(used_words, word, ());
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
  { cards: cards, hint: None, turn: 1 }
}

let component = ReasonReact.reducerComponent("Game");

let make = (~model, _children) => {
  ...component,
  initialState: () => create(model),
  reducer: (action, state) => {
    let get_hint = () => {
      Js.log("Hinting");
      let cards = List.filter((card) => {!List.mem(`Me_wrong, card.guess) && !List.mem(`Correct, card.guess)}, Array.to_list(state.cards));
      let cards = List.map((card) => (card.word, card.their_color), cards);
      Some(Hint.best(model, cards))
    };
    let next_turn = (state) => {
      if (List.exists((card) => (card.my_color == `Green || card.their_color == `Green) && !List.mem(`Correct, card.guess), Array.to_list(state.cards))) {
        let i_hint = () => {...state, hint: None, turn: state.turn + 1};
        let they_hint = () => {...state, hint: get_hint(), turn: state.turn + 1};
        switch (state.hint) {
        | None =>
          if (List.exists((card) => card.their_color == `Green && !List.mem(`Correct, card.guess), Array.to_list(state.cards))) {
            they_hint()
          } else {
            i_hint()
          }
        | Some(_) =>
          if (List.exists((card) => card.my_color == `Green && !List.mem(`Correct, card.guess), Array.to_list(state.cards))) {
            i_hint()
          } else {
            they_hint()
          }
        }
      } else {
        alert("You won.");
        state
      }
    };
    switch (action) {
    | New => ReasonReact.Update(create(model))
    | Pass => ReasonReact.Update(next_turn(state))
    | Guess(i) => {
      let card = state.cards[i];
      switch (card.their_color) {
      | `Green => {
        state.cards[i] = {...card, guess: [`Correct, ...card.guess]};
        ReasonReact.Update(state)
      }
      | `White => {
        state.cards[i] = {...card, guess: [`Me_wrong, ...card.guess]};
        ReasonReact.Update(next_turn(state))
      }
      | `Black => {
        state.cards[i] = {...card, guess: [`Me_wrong, ...card.guess]};
        alert("You lost.");
        ReasonReact.Update(state)
      }
      }
    }
    | Hint(word, n) => {
      let vec = (word) => model.vec[Hashtbl.find(model.dict, word)];
      let v = vec(word);
      let distances = Array.mapi((i, card) => {
          if (List.mem(`They_wrong, card.guess) || List.mem(`Correct, card.guess)) {
            (1000., i)
          } else {
            (distance(v, vec(card.word)), i)
          }
        }, state.cards);
      Array.sort(compare, distances);
      let rec guess = (i, n) => {
        if (i == n) {
          ReasonReact.Update(next_turn(state))
        } else {
          let j = snd(distances[i]);
          let card = state.cards[j];
          Js.log("guess " ++ card.word);
          switch (card.my_color) {
          | `Green => {
            state.cards[j] = {...card, guess: [`Correct, ...card.guess]};
            guess(i + 1, n)
          }
          | `White => {
            state.cards[j] = {...card, guess: [`They_wrong, ...card.guess]};
            ReasonReact.Update(next_turn(state))
          }
          | `Black => {
            state.cards[j] = {...card, guess: [`They_wrong, ...card.guess]};
            alert("You lost.");
            ReasonReact.Update(state)
          }
          }
        }
      };
      Js.log("Guessing " ++ string_of_int(n));
      guess(0, n)
    }
    }
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
        let color = switch (state.hint) {
        | None => card.my_color
        | Some(_) => `White
        };
        <Card classes=[(color :> card_class), ...(card.guess :> list(card_class))] onClick=click>...card.word</Card>
      }, state.cards);
    <>
      <div>
        <button onClick={(_event) => send(New)}>{ReasonReact.string("New game")}</button>
      </div>
      {switch (state.hint) {
      | None => <HintInput onSubmit={(word, n) => send(Hint(word, n))} model=model/>
      | Some((word, n)) =>
        <div>
          {ReasonReact.string(word ++ " " ++ string_of_int(n))}
          <br/>
          <button onClick={(_event) => send(Pass)}>{ReasonReact.string("Pass")}</button>
        </div>
      }}
      <div id="table">
        ...cards
      </div>
      {ReasonReact.string("Turn " ++ string_of_int(state.turn))}
    </>
  }
}

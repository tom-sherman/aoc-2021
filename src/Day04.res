@send external unsafe_arrayAt: (Js.Array2.t<'a>, int) => 'a = "at"

module BingoCard = {
  type number = {value: int, marked: bool}
  type t = {rows: Seq.t<Seq.t<number>>, id: string}

  let spaceRe = Js.Re.fromString("\s+")

  let make = str => {
    rows: str
    ->String.splitSeq("\n")
    ->Seq.map(line =>
      line
      ->Js.String2.trim
      ->Js.String2.splitByRe(spaceRe)
      ->Js.Array2.map(Belt.Option.getExn)
      ->Seq.fromArray
      ->Seq.map(value => {value: value->Int.fromString, marked: false})
    ),
    id: str->Js.String2.replaceByRe(Js.Re.fromStringWithFlags("\s+", ~flags="g"), ""),
  }

  let size = card => card->Seq.length

  let mark = (card, value) => {
    rows: card.rows->Seq.map(row =>
      row->Seq.map(number =>
        if number.value === value {
          {...number, marked: true}
        } else {
          number
        }
      )
    ),
    id: card.id,
  }

  let _isLineWinner = rows => rows->Seq.some(row => row->Seq.every(({marked}) => marked))

  let isWinner = card => card.rows->_isLineWinner || card.rows->Seq.transpose->_isLineWinner

  let getMarked = card => card.rows->Seq.flatMap(row => row->Seq.filter(({marked}) => marked))
  let getUnarked = card => card.rows->Seq.flatMap(row => row->Seq.filter(({marked}) => !marked))

  let toArray = card => card.rows->Seq.map(Seq.toArray)->Seq.toArray
  let toString = card =>
    card.rows
    ->Seq.map(row =>
      row
      ->Seq.map(({marked, value}) =>
        `${if marked {
            `✅`
          } else {
            `❌`
          }}${value->string_of_int->String.padStart(2, "0")}`
      )
      ->Seq.toArray
      ->Js.Array2.joinWith(" ")
    )
    ->Seq.toArray
    ->Js.Array2.joinWith("\n")
}

type parseResult = {draws: Seq.t<int>, cards: Seq.t<BingoCard.t>}

let parseInput = input => {
  let (draws, cardStrings) = input->String.splitSeq("\n\n")->Seq.uncons->Belt.Option.getExn
  let draws = draws->String.splitSeq(",")->Seq.map(Int.fromString)
  let cards = cardStrings->Seq.map(BingoCard.make)

  {draws: draws, cards: cards}
}

let getWinners = (draws, cards: Seq.t<BingoCard.t>) =>
  draws
  ->Seq.scan(([], cards), ((winners, cards), draw) => {
    let newCards = cards->Seq.map(card => card->BingoCard.mark(draw))

    let newWinners =
      newCards
      ->Seq.filterMap(card =>
        if card->BingoCard.isWinner {
          Some((card, draw))
        } else {
          None
        }
      )
      ->Seq.toArray

    (
      winners->Js.Array2.concat(newWinners),
      if newWinners->Js.Array2.length === 0 {
        newCards
      } else {
        newCards->Seq.filter(card =>
          !(newWinners->Js.Array2.some(((winner, _)) => winner.id === card.id))
        )
      },
    )
  })
  ->Seq.filterMap(((winners, _)) =>
    if winners->Js.Array2.length === 0 {
      None
    } else {
      Some(winners)
    }
  )

let solveDay1 = input => {
  let {draws, cards} = parseInput(input)

  let (winningCard, winningDraw) =
    getWinners(draws, cards)->Seq.get(0)->Belt.Option.getExn->unsafe_arrayAt(0)

  winningCard->BingoCard.getUnarked->Seq.reduce(0, (acc, {value}) => acc + value) * winningDraw
}

let solveDay2 = input => {
  let {draws, cards} = parseInput(input)

  let (winningCard, winningDraw) =
    getWinners(draws, cards)->Seq.reverse->Seq.get(0)->Belt.Option.getExn->unsafe_arrayAt(-1)

  winningCard->BingoCard.getUnarked->Seq.reduce(0, (acc, {value}) => acc + value) * winningDraw
}

let solve = (input, part) =>
  switch part {
  | "1" => solveDay1(input)
  | "2" => solveDay2(input)
  | _ => Js.Exn.raiseError(`Unhandled part ${part}`)
  }

module BingoCard = {
  type number = {value: int, marked: bool}
  type t = Seq.t<Seq.t<number>>

  let spaceRe = Js.Re.fromString("\s+")

  let make = str =>
    str
    ->String.splitSeq("\n")
    ->Seq.map(line =>
      line
      ->Js.String2.trim
      ->Js.String2.splitByRe(spaceRe)
      ->Js.Array2.map(Belt.Option.getExn)
      ->Seq.fromArray
      ->Seq.map(value => {value: value->Int.fromString, marked: false})
    )

  let size = card => card->Seq.length

  let mark = (card, value) =>
    card->Seq.map(row =>
      row->Seq.map(number =>
        if number.value === value {
          {...number, marked: true}
        } else {
          number
        }
      )
    )

  let isLineWinner = card => card->Seq.some(row => row->Seq.every(({marked}) => marked))

  let isWinner = card => card->isLineWinner || card->Seq.transpose->isLineWinner

  let getMarked = card => card->Seq.flatMap(row => row->Seq.filter(({marked}) => marked))
  let getUnarked = card => card->Seq.flatMap(row => row->Seq.filter(({marked}) => !marked))

  let toArray = (card: Seq.t<Seq.t<number>>) => card->Seq.map(Seq.toArray)->Seq.toArray
}

type parseResult = {draws: Seq.t<int>, cards: Seq.t<BingoCard.t>}

let parseInput = input => {
  let (draws, cardStrings) = input->String.splitSeq("\n\n")->Seq.uncons->Belt.Option.getExn
  let draws = draws->String.splitSeq(",")->Seq.map(Int.fromString)
  let cards = cardStrings->Seq.map(BingoCard.make)

  {draws: draws, cards: cards}
}

let getWinners = (draws, cards) =>
  draws
  ->Seq.scan((None, cards), ((_, cards), draw) => {
    let newCards = cards->Seq.map(card => card->BingoCard.mark(draw))

    let maybeWinner =
      newCards
      ->Seq.find(card => card->BingoCard.isWinner)
      ->Belt.Option.map(winner => (winner, draw))

    (maybeWinner, newCards)
  })
  ->Seq.filterMap(((maybeWinner, _)) => maybeWinner)

let solveDay1 = input => {
  let {draws, cards} = parseInput(input)

  let (winningCard, winningDraw) = getWinners(draws, cards)->Seq.get(0)->Belt.Option.getExn

  winningCard->BingoCard.getUnarked->Seq.reduce(0, (acc, {value}) => acc + value) * winningDraw
}

let solveDay2 = input => {
  0
}

let testData = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"

let solve = (input, part) =>
  switch part {
  | "1" => solveDay1(input)
  | "2" => solveDay2(testData)
  | _ => Js.Exn.raiseError(`Unhandled part ${part}`)
  }

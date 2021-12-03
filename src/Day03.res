@val external parseIntWithRadix: (string, ~radix: int) => int = "parseInt"

type sum = {countOnes: int, countZeros: int}
type rates = {epsilon: string, gamma: string}

let makeSum = word =>
  word->Seq.reduce({countOnes: 0, countZeros: 0}, (sum, char) =>
    switch char {
    | "0" => {...sum, countZeros: sum.countZeros + 1}
    | "1" => {...sum, countOnes: sum.countOnes + 1}
    | _ => Js.Exn.raiseError("Couldn't parse char")
    }
  )

let solveDay1 = input => {
  let columns = input->String.splitSeq("\n")->Seq.map(String.explodeSeq)->Seq.transpose

  let allSums = columns->Seq.map(makeSum)
  let {gamma, epsilon} = allSums->Seq.reduce({gamma: "", epsilon: ""}, (
    {gamma, epsilon},
    {countOnes, countZeros},
  ) => {
    let newGammaChar = if countOnes > countZeros {
      "1"
    } else {
      "0"
    }

    let newEpsilonChar = if countOnes > countZeros {
      "0"
    } else {
      "1"
    }

    {
      gamma: `${gamma}${newGammaChar}`,
      epsilon: `${epsilon}${newEpsilonChar}`,
    }
  })

  gamma->parseIntWithRadix(~radix=2) * epsilon->parseIntWithRadix(~radix=2)
}
let solveDay2 = input => {
  let rec filter = (words, position, bitCritieria) =>
    switch words->Seq.uncons {
    | None => Js.Exn.raiseError("Should not happen")
    | Some(firstWord, rest) =>
      if rest->Seq.isEmpty {
        firstWord
      } else {
        let sum = makeSum(words->Seq.transpose->Seq.get(position)->Belt.Option.getExn)
        let newWords =
          words->Seq.filter(word => bitCritieria(sum, word->Seq.get(position)->Belt.Option.getExn))

        filter(newWords, position + 1, bitCritieria)
      }
    }

  let words = input->String.splitSeq("\n")->Seq.map(String.explodeSeq)

  let generatorBitCriteria = ({countOnes, countZeros}, char) =>
    if countOnes === countZeros {
      char === "1"
    } else if countOnes > countZeros {
      char === "1"
    } else {
      char === "0"
    }

  let scrubberBitCriteria = ({countOnes, countZeros}, char) =>
    if countOnes === countZeros {
      char === "0"
    } else if countOnes > countZeros {
      char === "0"
    } else {
      char === "1"
    }

  let generatorRating =
    filter(words, 0, generatorBitCriteria)
    ->Seq.reduce("", (a, b) => a ++ b)
    ->parseIntWithRadix(~radix=2)

  let scrubberRating =
    filter(words, 0, scrubberBitCriteria)
    ->Seq.reduce("", (a, b) => a ++ b)
    ->parseIntWithRadix(~radix=2)

  generatorRating * scrubberRating
}

let solve = (input, part) =>
  switch part {
  | "1" => solveDay1(input)
  | "2" => solveDay2(input)
  | _ => Js.Exn.raiseError(`Unhandled part ${part}`)
  }

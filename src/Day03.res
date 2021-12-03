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

let solve = solveDay1

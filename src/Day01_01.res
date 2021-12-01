let intOfBool = b =>
  if b {
    1
  } else {
    0
  }

let rec countAdjacentIncreasingElements = (s, count) =>
  switch s->Seq.uncons {
  | None => count
  | Some(current, tail) =>
    switch tail->Seq.uncons {
    | None => count
    | Some(next, _) => countAdjacentIncreasingElements(tail, count + intOfBool(next > current))
    }
  }

let solve = input =>
  input
  ->Js.String2.split("\n")
  ->Seq.fromArray
  ->Seq.map(line => line->Belt.Int.fromString->Belt.Option.getExn)
  ->countAdjacentIncreasingElements(0)

let rec countAdjacentIncreasingElements = (s, count) =>
  switch s->Seq.uncons {
  | None => count
  | Some(current, tail) =>
    switch tail->Seq.uncons {
    | None => count
    | Some(next, _) => countAdjacentIncreasingElements(tail, count + Bool.toInt(next > current))
    }
  }

let solveDay1 = input =>
  input
  ->String.splitSeq("\n")
  ->Seq.map(line => line->Belt.Int.fromString->Belt.Option.getExn)
  ->countAdjacentIncreasingElements(0)

let rec windows = (xs, n) => {
  let window = xs->Seq.take(n)

  if n > window->Seq.length {
    Seq.empty
  } else {
    switch xs->Seq.uncons {
    | None => Seq.empty
    | Some(_, xs) => windows(xs, n)->Seq.prepend(window)
    }
  }
}

let sum = xs => xs->Seq.reduce(0, (a, b) => a + b)

let solveDay2 = input =>
  input
  ->String.splitSeq("\n")
  ->Seq.map(line => line->Belt.Int.fromString->Belt.Option.getExn)
  ->windows(3)
  ->Seq.map(sum)
  ->countAdjacentIncreasingElements(0)

let solve = (input, part) =>
  switch part {
  | "1" => solveDay1(input)
  | "2" => solveDay2(input)
  | _ => Js.Exn.raiseError(`Unhandled part ${part}`)
  }

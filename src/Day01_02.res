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

let solve = input =>
  input
  ->Js.String2.split("\n")
  ->Seq.fromArray
  ->Seq.map(line => line->Belt.Int.fromString->Belt.Option.getExn)
  ->windows(3)
  ->Seq.map(sum)
  ->Day01_01.countAdjacentIncreasingElements(0)

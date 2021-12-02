@val external parseInt: string => int = "parseInt"

type move = Forward(int) | Down(int) | Up(int)

let parse = input =>
  input
  ->String.splitSeq("\n")
  ->Seq.map(line =>
    switch line->Js.String2.split(" ") {
    | [direction, amount] =>
      switch direction {
      | "forward" => Forward(parseInt(amount))
      | "up" => Up(parseInt(amount))
      | "down" => Down(parseInt(amount))
      | _ => Js.Exn.raiseError("Couldn't parse line")
      }
    | _ => Js.Exn.raiseError("Couldn't parse line")
    }
  )

let addPair = ((a, b), (c, d)) => (a + c, b + d)
let pairProduct = ((a, b)) => a * b

let solveDay1 = input =>
  input
  ->parse
  ->Seq.map(move =>
    switch move {
    | Forward(n) => (n, 0)
    | Down(n) => (0, n)
    | Up(n) => (0, -n)
    }
  )
  ->Seq.reduce((0, 0), addPair)
  ->pairProduct

type position = {horizontal: int, depth: int, aim: int}

let solveDay2 = input =>
  input
  ->parse
  ->Seq.reduce({horizontal: 0, depth: 0, aim: 0}, (pos, move) =>
    switch move {
    | Down(n) => {...pos, aim: pos.aim + n}
    | Up(n) => {...pos, aim: pos.aim - n}
    | Forward(n) => {...pos, horizontal: pos.horizontal + n, depth: pos.depth + pos.aim * n}
    }
  )
  ->(pos => pos.depth * pos.horizontal)

let solve = (input, part) =>
  switch part {
  | "1" => solveDay1(input)
  | "2" => solveDay2(input)
  | _ => Js.Exn.raiseError(`Unhandled part ${part}`)
  }

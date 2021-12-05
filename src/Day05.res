let lineRe = Js.Re.fromString("(\d+),(\d+) -> (\d+),(\d+)")

type line = {x1: int, y1: int, x2: int, y2: int}

let parse = input =>
  input
  ->String.splitSeq("\n")
  ->Seq.map(line =>
    switch line->Js.String2.split("->")->Js.Array2.map(Js.String2.trim) {
    | [xs, ys] =>
      switch (xs->Js.String2.split(","), ys->Js.String2.split(",")) {
      | ([x1, y1], [x2, y2]) => {
          x1: x1->Int.fromString,
          y1: y1->Int.fromString,
          x2: x2->Int.fromString,
          y2: y2->Int.fromString,
        }
      | _ => Js.Exn.raiseError("Couldn't parse line")
      }
    | _ => Js.Exn.raiseError("Couldn't parse line")
    }
  )

let direction = (a, b) =>
  if a > b {
    -1
  } else if a < b {
    1
  } else {
    0
  }

let lineRange = line =>
  Seq.unfold(Some(line), maybeLine =>
    maybeLine->Belt.Option.map(({x1, x2, y1, y2}) => {
      let xDirection = direction(x1, x2)
      let yDirection = direction(y1, y2)

      switch (x1 === x2, y1 === y2) {
      | (true, true) => ((x1, y1), None)
      | (true, false) => {
          let newY = y1 + yDirection
          ((x1, y1), Some({x1: x1, x2: x2, y1: newY, y2: y2}))
        }
      | (false, true) => {
          let newX = x1 + xDirection
          ((x1, y1), Some({x1: newX, x2: x2, y1: y1, y2: y2}))
        }
      | (false, false) => {
          let newX = x1 + xDirection
          let newY = y1 + yDirection
          ((x1, y1), Some({x1: newX, x2: x2, y1: newY, y2: y2}))
        }
      }
    })
  )

let solveDay1 = input => {
  let lines = input->parse->Seq.filter(line => line.x1 === line.x2 || line.y1 === line.y2)
  let intersections = ref(0)
  module Map = Belt.MutableMap.Int
  let map = Map.make()

  lines->Seq.iter(line =>
    line
    ->lineRange
    ->Seq.iter(((x, y)) => {
      let row = map->Map.get(y)->Belt.Option.getWithDefault(Map.make())
      switch row->Map.get(x) {
      | None => {
          row->Map.set(x, 0)
          map->Map.set(y, row)
        }
      | Some(intersectionCount) =>
        if intersectionCount === 0 {
          intersections := intersections.contents + 1
          row->Map.set(x, 1)
          map->Map.set(y, row)
        }
      }
    })
  )

  intersections.contents
}

let solveDay2 = input => {
  let lines = input->parse
  let intersections = ref(0)
  module Map = Belt.MutableMap.Int
  let map = Map.make()

  lines->Seq.iter(line =>
    line
    ->lineRange
    ->Seq.iter(((x, y)) => {
      let row = map->Map.get(y)->Belt.Option.getWithDefault(Map.make())
      switch row->Map.get(x) {
      | None => {
          row->Map.set(x, 0)
          map->Map.set(y, row)
        }
      | Some(intersectionCount) =>
        if intersectionCount === 0 {
          intersections := intersections.contents + 1
          row->Map.set(x, 1)
          map->Map.set(y, row)
        }
      }
    })
  )

  intersections.contents
}

let solve = (input, part) =>
  switch part {
  | "1" => solveDay1(input)
  | "2" => solveDay2(input)
  | _ => Js.Exn.raiseError(`Unhandled part ${part}`)
  }

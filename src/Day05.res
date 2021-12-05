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

module Map = Belt.Map.Int

let getIntersectionCount = lines =>
  lines
  ->Seq.reduce((0, Map.empty), ((intersections, map), line) => {
    let range = lineRange(line)

    range->Seq.reduce((intersections, map), ((intersections, map), (x, y)) => {
      let row = map->Map.get(y)->Belt.Option.getWithDefault(Map.empty)
      let (newIntersections, newRow) = switch row->Map.get(x) {
      | None => (intersections, row->Map.set(x, 0))
      | Some(intersectionCount) =>
        if intersectionCount === 0 {
          (intersections + 1, row->Map.set(x, 1))
        } else {
          (intersections, row)
        }
      }

      (newIntersections, map->Map.set(y, newRow))
    })
  })
  ->fst

let solveDay1 = input =>
  input->parse->Seq.filter(line => line.x1 === line.x2 || line.y1 === line.y2)->getIntersectionCount

let solveDay2 = input => input->parse->getIntersectionCount

let solve = (input, part) =>
  switch part {
  | "1" => solveDay1(input)
  | "2" => solveDay2(input)
  | _ => Js.Exn.raiseError(`Unhandled part ${part}`)
  }

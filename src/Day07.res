let floatCompare = (a, b) => {
  let diff = a -. b
  if diff == 0.0 {
    0
  } else if diff > 0.0 {
    1
  } else {
    -1
  }
}

let median = arr => {
  let arr = arr->Js.Array2.sliceFrom(0)->Js.Array2.sortInPlaceWith(floatCompare)
  let count = Js.Array.length(arr)
  let middleval = (count - 1) / 2
  let median = if mod(count, 2) != 0 {
    arr[middleval]
  } else {
    let low = arr[middleval]
    let high = arr[middleval + 1]
    (low +. high) /. 2.0
  }
  median
}

let solveDay1 = input => {
  let positions = input->Js.String2.split(",")->Js.Array2.map(Js.Float.fromString)
  let target = positions->Js.Array2.sliceFrom(0)->Js.Array2.sortInPlaceWith(floatCompare)->median

  positions->Js.Array2.reduce((sum, position) => sum +. Js.Math.abs_float(position -. target), 0.)
}

let solveDay2 = input => {
  let positions = input->Js.String2.split(",")->Js.Array2.map(Js.Float.fromString)
  Js.log(positions->Js.Array2.sortInPlaceWith(floatCompare))
  // let target = positions->median

  1.
}

let solve = (input, part) =>
  switch part {
  | "1" => solveDay1(input)
  | "2" => solveDay2("16,1,2,0,4,2,7,1,2,14")
  | _ => Js.Exn.raiseError(`Unhandled part ${part}`)
  }

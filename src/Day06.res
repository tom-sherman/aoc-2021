let range = (start, length) => Seq.ints(start)->Seq.take(length)->Seq.toArray

let _solve = (input, days) => {
  let fish = input->Js.String2.split(",")->Js.Array2.map(Js.Float.fromString)
  let generations =
    range(0, 9)->Js.Array2.map(i =>
      fish->Js.Array2.filter(n => n === i->float_of_int)->Js.Array2.length->float_of_int
    )

  for _ in 1 to days {
    let spawns = generations->Js.Array2.shift->Belt.Option.getUnsafe
    generations->Js.Array2.unsafe_set(6, generations->Js.Array2.unsafe_get(6) +. spawns)
    generations->Js.Array2.push(spawns)->ignore
  }

  generations->Js.Array2.reduce((a, b) => a +. b, 0.0)
}

let solve = (input, part) =>
  switch part {
  | "1" => _solve(input, 80)
  | "2" => _solve(input, 256)
  | _ => Js.Exn.raiseError(`Unhandled part ${part}`)
  }

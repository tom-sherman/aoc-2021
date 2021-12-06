// let timerValue = (start, n) => {
//   let n = n + Js.Math.max_int(0, n - 8)
//   let start = start - Js.Math.max_int(0, n - 8)
//   let result = start - n
//   if result >= 0 {
//     result
//   } else {
//     6 - mod(6 - result, 6) + 1
//   }
// }

// Js.log(timerValue(8, 1))
// Js.log(timerValue(8, 2))
// Js.log(timerValue(8, 3))
// Js.log(timerValue(8, 4))
// Js.log(timerValue(8, 5))
// Js.log(timerValue(8, 6))
// Js.log(timerValue(8, 7))
// Js.log(timerValue(8, 8))
// Js.log(timerValue(8, 9))
// Js.log(timerValue(8, 10))

let _solve = (input, days) => {
  // let rec aux = (pos, fish, count) => {
  //   let newCount = ref(0)

  //   Belt.Range.forEach(pos, days, day => {
  //     ()
  //   })

  //   count + newCount.contents
  // }

  let fish = ref(input->Js.String2.split(",")->Js.Array2.map(Int.fromString))

  Belt.Range.forEach(1, days, _ => {
    let fishToAdd = ref(0)
    let newFish = fish.contents->Js.Array2.map(timer =>
      if timer === 0 {
        fishToAdd := fishToAdd.contents + 1
        6
      } else {
        timer - 1
      }
    )

    Belt.Range.forEach(1, fishToAdd.contents, _ => newFish->Js.Array2.push(8)->ignore)

    fish := newFish
  })

  fish.contents->Js.Array2.length
}

let solve = (input, part) =>
  switch part {
  | "1" => _solve(input, 80)
  | "2" => _solve(input, 256)
  | _ => Js.Exn.raiseError(`Unhandled part ${part}`)
  }

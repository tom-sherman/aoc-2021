let get = (str, i) => {
  let val = str->Js.String2.get(i)
  if Js.Undefined.testAny(val) {
    None
  } else {
    Some(val)
  }
}

let length = Js.String2.length

// FIXME: Doesn't handle multi codepoint unicode chars
let explodeSeq = (str: string): Seq.t<string> =>
  Seq.unfold(0, i => str->get(i)->Belt.Option.map(x => (x, i + 1)))

// TODO: Make lazy / don't allocate an extra array
let splitSeq = (str, delimiter) => str->Js.String2.split(delimiter)->Seq.fromArray

let parseChars = (string): array<array<string>> =>
  string->Js.String2.split(" ")->Js.Array2.map(string => string->Js.String2.split(""))

let parse = string =>
  string
  ->String.splitSeq("\n")
  ->Seq.map(line => {
    switch line->Js.String2.split(" | ") {
    | [input, output] => (input->parseChars, output->parseChars)
    | _ => Js.Exn.raiseError("Couldn't parse line")
    }
  })

module StringSet = Belt.Set.String

type segmentLookup = {
  a: StringSet.t,
  b: StringSet.t,
  c: StringSet.t,
  d: StringSet.t,
  e: StringSet.t,
  f: StringSet.t,
  g: StringSet.t,
}

let emptyDigitLookup = {
  a: StringSet.empty,
  b: StringSet.empty,
  c: StringSet.empty,
  d: StringSet.empty,
  e: StringSet.empty,
  f: StringSet.empty,
  g: StringSet.empty,
}

let isLookupEmpty = lookup =>
  lookup.a->StringSet.size === 0 &&
  lookup.b->StringSet.size === 0 &&
  lookup.c->StringSet.size === 0 &&
  lookup.d->StringSet.size === 0 &&
  lookup.e->StringSet.size === 0 &&
  lookup.f->StringSet.size === 0 &&
  lookup.g->StringSet.size === 0

type unambiguousSegmentLookup = {
  a: string,
  b: string,
  c: string,
  d: string,
  e: string,
  f: string,
  g: string,
}

let unambiguize = (lookup: segmentLookup) =>
  if (
    lookup.a->StringSet.size === 1 &&
    lookup.b->StringSet.size === 1 &&
    lookup.c->StringSet.size === 1 &&
    lookup.d->StringSet.size === 1 &&
    lookup.e->StringSet.size === 1 &&
    lookup.f->StringSet.size === 1 &&
    lookup.g->StringSet.size === 1
  ) {
    Some({
      a: (lookup.a->StringSet.toArray)[0],
      b: (lookup.b->StringSet.toArray)[0],
      c: (lookup.c->StringSet.toArray)[0],
      d: (lookup.d->StringSet.toArray)[0],
      e: (lookup.e->StringSet.toArray)[0],
      f: (lookup.f->StringSet.toArray)[0],
      g: (lookup.g->StringSet.toArray)[0],
    })
  } else {
    None
  }

let inferLookup = chars => {
  let s = chars->StringSet.fromArray
  switch chars->Js.Array2.length {
  | 2 => {...emptyDigitLookup, b: s, c: s}
  | 3 => {...emptyDigitLookup, a: s, b: s, c: s}
  | 4 => {...emptyDigitLookup, b: s, c: s, f: s, g: s}
  | 7 => {a: s, b: s, c: s, d: s, e: s, f: s, g: s}
  | _ => emptyDigitLookup
  }
}

let mergeLookups = (lookupA: segmentLookup, lookupB: segmentLookup): segmentLookup => {
  {
    a: StringSet.union(lookupA.a, lookupB.a),
    b: StringSet.union(lookupA.b, lookupB.b),
    c: StringSet.union(lookupA.c, lookupB.c),
    d: StringSet.union(lookupA.d, lookupB.d),
    e: StringSet.union(lookupA.e, lookupB.e),
    f: StringSet.union(lookupA.f, lookupB.f),
    g: StringSet.union(lookupA.g, lookupB.g),
  }
}

type sums = {ones: int, fours: int, sevens: int, eights: int}

let solveDay1 = input => {
  let lines = input->parse

  let sums = lines->Seq.reduce({ones: 0, fours: 0, sevens: 0, eights: 0}, (sums, (_, output)) =>
    output->Js.Array2.reduce((sums, chars) =>
      switch chars->Js.Array2.length {
      | 2 => {...sums, ones: sums.ones + 1}
      | 3 => {...sums, sevens: sums.sevens + 1}
      | 4 => {...sums, fours: sums.fours + 1}
      | 7 => {...sums, eights: sums.eights + 1}
      | _ => sums
      }
    , sums)
  )

  sums.ones + sums.fours + sums.sevens + sums.eights
}

let solveDay2 = input => {
  let lines = input->parse
  let x =
    lines->Seq.scan(emptyDigitLookup, (lookup, (digitChars, _)) =>
      digitChars->Js.Array2.reduce(
        (lookup, chars) => mergeLookups(lookup, inferLookup(chars)),
        lookup,
      )
    )
  // ->Seq.findMap(unambiguize)
  // ->Belt.Option.getExn

  Js.log(
    x
    ->Seq.reverse
    ->Seq.get(0)
    ->Belt.Option.map(l =>
      {
        "a": l.a->StringSet.toArray,
        "b": l.b->StringSet.toArray,
        "c": l.c->StringSet.toArray,
        "d": l.d->StringSet.toArray,
        "e": l.e->StringSet.toArray,
        "f": l.f->StringSet.toArray,
        "g": l.g->StringSet.toArray,
      }
    ),
  )

  0
}

let exampleInput = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

let solve = (input, part) =>
  switch part {
  | "1" => solveDay1(input)
  | "2" => solveDay2(input)
  | _ => Js.Exn.raiseError(`Unhandled part ${part}`)
  }

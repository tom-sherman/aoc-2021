let exchange = (r, v) => {
  let cur = r.contents
  r.contents = v
  cur
}

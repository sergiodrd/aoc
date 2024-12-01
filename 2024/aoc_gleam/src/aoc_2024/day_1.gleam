import gleam/int
import gleam/list
import gleam/string

pub fn pt_1(input: String) -> Int {
  let #(al, bl) =
    input
    |> string.split(on: "\n")
    |> list.map(fn(line) {
      let assert [a, b] = line |> string.split("   ")
      let assert [Ok(a), Ok(b)] = [int.parse(a), int.parse(b)]
      #(a, b)
    })
    |> list.unzip()

  let #(al, bl) = #(
    list.sort(al, by: int.compare),
    list.sort(bl, by: int.compare),
  )

  let assert Ok(n) =
    list.map2(al, bl, fn(a, b) { int.absolute_value(a - b) })
    |> list.reduce(int.add)

  n
}

pub fn pt_2(input: String) -> Int {
  let #(al, bl) =
    input
    |> string.split(on: "\n")
    |> list.map(fn(line) {
      let assert [a, b] = line |> string.split("   ")
      let assert [Ok(a), Ok(b)] = [int.parse(a), int.parse(b)]
      #(a, b)
    })
    |> list.unzip()

  al
  |> list.fold(0, fn(acc, n) {
    acc + n * { bl |> list.count(fn(x) { x == n }) }
  })
}

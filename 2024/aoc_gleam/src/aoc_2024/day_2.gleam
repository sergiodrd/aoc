import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

fn get_matrix(input: String) -> List(List(Int)) {
  input
  |> string.split(on: "\n")
  |> list.map(fn(str) {
    str
    |> string.split(on: " ")
    |> list.map(fn(x) {
      let assert Ok(x) = int.parse(x)
      x
    })
  })
}

fn determine_safe(levels: List(Int)) -> Bool {
  let diffs =
    list.window_by_2(levels)
    |> list.map(fn(t) { t.0 - t.1 })

  let monotonic =
    list.window_by_2(diffs)
    |> list.all(fn(t) { t.0 >= 0 && t.1 >= 0 || t.0 <= 0 && t.1 <= 0 })

  let gradual =
    diffs
    |> list.all(fn(x) {
      int.absolute_value(x) >= 1 && int.absolute_value(x) <= 3
    })

  monotonic && gradual
}

pub fn pt_1(input: String) -> Int {
  list.fold(get_matrix(input), 0, fn(acc, levels) {
    case determine_safe(levels) {
      True -> acc + 1
      False -> acc
    }
  })
}

type RemovalList(a) {
  RemovalList(l: List(a), r: List(a))
}

fn fold_removals(l: RemovalList(a), a: acc, f: fn(acc, List(a)) -> acc) -> acc {
  case l {
    RemovalList(_, []) -> a
    RemovalList(l, [x, ..rest]) ->
      fold_removals(
        RemovalList(list.append(l, [x]), rest),
        f(a, list.append(l, rest)),
        f,
      )
  }
}

pub fn pt_2(input: String) {
  list.fold(get_matrix(input), 0, fn(acc, levels) {
    case determine_safe(levels) {
      True -> acc + 1
      False ->
        case
          fold_removals(RemovalList([], levels), False, fn(acc, removed) {
            acc || determine_safe(removed)
          })
        {
          True -> acc + 1
          False -> acc
        }
    }
  })
}

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/order
import gleam/result
import gleam/set.{type Set}
import gleam/string

fn all_with_rest(l: List(a), f: fn(a, List(a)) -> Bool) -> Bool {
  case l {
    [] -> True
    [x, ..rest] ->
      case f(x, rest) {
        True -> all_with_rest(rest, f)
        False -> False
      }
  }
}

fn parse(input: String) -> #(Dict(Int, Set(Int)), List(List(Int))) {
  let #(rules, updates) =
    input
    |> string.split("\n")
    |> list.split_while(fn(x) { x != "" })

  let rules =
    rules
    |> list.fold(dict.new(), fn(rules, x) {
      let assert [a, b] = x |> string.split(on: "|")
      let assert #(Ok(a), Ok(b)) = #(a |> int.parse, b |> int.parse)
      rules
      |> dict.upsert(a, fn(set) {
        case set {
          None -> set.new() |> set.insert(b)
          Some(set) -> set |> set.insert(b)
        }
      })
    })

  let assert Ok(updates) = list.rest(updates)
  let updates =
    updates
    |> list.map(fn(pages) {
      string.split(pages, on: ",")
      |> list.map(fn(x) {
        let assert Ok(x) = int.parse(x)
        x
      })
    })

  #(rules, updates)
}

pub fn pt_1(input: String) -> Int {
  let #(rules, updates) = parse(input)

  updates
  |> list.filter_map(fn(pages) {
    let ordered =
      all_with_rest(pages, fn(page, rest) {
        use x <- list.all(rest)
        case rules |> dict.get(x) {
          Ok(set) -> !{ set |> set.contains(page) }
          Error(_) -> True
        }
      })

    case ordered {
      False -> Error(0)
      True ->
        Ok(
          pages
          |> list.drop(list.length(pages) / 2)
          |> list.first()
          |> result.unwrap(0),
        )
    }
  })
  |> int.sum
}

pub fn pt_2(input: String) {
  let #(rules, updates) = parse(input)

  updates
  |> list.filter_map(fn(pages) {
    let ordered =
      all_with_rest(pages, fn(page, rest) {
        use x <- list.all(rest)
        case rules |> dict.get(x) {
          Ok(set) -> !{ set |> set.contains(page) }
          Error(_) -> True
        }
      })

    case ordered {
      True -> Error(0)
      False ->
        Ok(
          pages
          |> list.sort(fn(x, y) {
            case
              rules
              |> dict.get(x)
              |> result.unwrap(set.new())
              |> set.contains(y),
              rules
              |> dict.get(y)
              |> result.unwrap(set.new())
              |> set.contains(x)
            {
              True, False -> order.Lt
              False, True -> order.Gt
              False, False -> order.Eq
              True, True -> panic as "oh no"
            }
          })
          |> list.drop(list.length(pages) / 2)
          |> list.first()
          |> result.unwrap(0),
        )
    }
  })
  |> int.sum
}

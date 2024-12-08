import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

fn parse(input: String) -> List(#(Int, List(Int))) {
  input
  |> string.split(on: "\n")
  |> list.map(fn(line) {
    let assert [target, nums] = line |> string.split(on: ": ")
    let assert Ok(target) = int.parse(target)
    let nums =
      nums
      |> string.split(on: " ")
      |> list.map(fn(x) {
        let assert Ok(x) = int.parse(x)
        x
      })
    #(target, nums)
  })
}

fn can_satisfy_1(
  current: Option(Int),
  target: Int,
  remaining: List(Int),
) -> Bool {
  case current, target, remaining {
    None, _, [] -> panic as "unreachable!"
    None, t, [x, ..rest] -> can_satisfy_1(Some(x), t, rest)
    Some(x), t, [] -> x == t
    Some(x), t, [n, ..rest] ->
      case can_satisfy_1(Some(x * n), t, rest) {
        True -> True
        False -> can_satisfy_1(Some(x + n), t, rest)
      }
  }
}

fn concat(x: Int, y: Int) -> Int {
  let #(x, y) = #(int.to_string(x), int.to_string(y))
  let assert Ok(result) = string.concat([x, y]) |> int.parse
  result
}

fn can_satisfy_2(
  current: Option(Int),
  target: Int,
  remaining: List(Int),
) -> Bool {
  case current, target, remaining {
    None, _, [] -> panic as "unreachable!"
    None, t, [x, ..rest] -> can_satisfy_2(Some(x), t, rest)
    Some(x), t, [] -> x == t
    Some(x), t, [n, ..rest] ->
      case can_satisfy_2(Some(x * n), t, rest) {
        True -> True
        False ->
          case can_satisfy_2(Some(x + n), t, rest) {
            True -> True
            False -> can_satisfy_2(Some(concat(x, n)), t, rest)
          }
      }
  }
}

pub fn pt_1(input: String) -> Int {
  input
  |> parse
  |> list.filter_map(fn(line) {
    let #(target, nums) = line
    case can_satisfy_1(None, target, nums) {
      True -> Ok(target)
      False -> Error(0)
    }
  })
  |> int.sum
}

pub fn pt_2(input: String) {
  input
  |> parse
  |> list.filter_map(fn(line) {
    let #(target, nums) = line
    case can_satisfy_2(None, target, nums) {
      True -> Ok(target)
      False -> Error(0)
    }
  })
  |> int.sum
}

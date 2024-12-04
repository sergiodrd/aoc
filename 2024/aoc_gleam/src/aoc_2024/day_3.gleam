import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import nibble.{type Parser, Break, Continue, return}
import nibble/lexer.{type Lexer}
import nibble/predicates

pub type T {
  Mul
  Do
  Dont
  LParen
  RParen
  Comma
  Int(Int)
  Other
}

pub type Op {
  Multiply(Int, Int)
  Enable
  Disable
}

fn lexer() -> Lexer(T, Nil) {
  lexer.simple([
    lexer.custom(fn(_, c, n) {
      case c, n {
        "m", "u" -> lexer.Skip
        "mu", "l" -> lexer.Skip
        "mul", _ -> lexer.Keep(Mul, Nil)
        _, _ -> lexer.NoMatch
      }
    }),
    lexer.custom(fn(_, c, n) {
      case c, n {
        "d", "o" -> lexer.Skip
        "do", "n" -> lexer.Skip
        "do", _ -> lexer.Keep(Do, Nil)
        "don", "'" -> lexer.Skip
        "don'", "t" -> lexer.Skip
        "don't", _ -> lexer.Keep(Dont, Nil)
        _, _ -> lexer.NoMatch
      }
    }),
    lexer.token("(", LParen),
    lexer.token(")", RParen),
    lexer.token(",", Comma),
    lexer.custom(fn(_, c, n) {
      let are_digits =
        c |> string.to_graphemes |> list.all(fn(c) { predicates.is_digit(c) })
      case are_digits, predicates.is_digit(n) {
        False, _ -> lexer.NoMatch
        True, True -> lexer.Skip
        True, False -> {
          case int.parse(c) {
            Ok(n) -> lexer.Keep(Int(n), Nil)
            Error(_) -> lexer.NoMatch
          }
        }
      }
    }),
    lexer.keep(fn(_, _) { Ok(Other) }),
  ])
}

fn int_parser() -> Parser(Int, T, a) {
  use tok <- nibble.take_map("Expected an int")
  case tok {
    Int(n) -> Some(n)
    _ -> None
  }
}

fn parser() -> Parser(List(Op), T, a) {
  use operations <- nibble.loop([])
  nibble.one_of([
    // try parsing the Mul operation
    {
      use _ <- nibble.do(nibble.token(Mul))
      use _ <- nibble.do(nibble.token(LParen))
      use x <- nibble.do(int_parser())
      use _ <- nibble.do(nibble.token(Comma))
      use y <- nibble.do(int_parser())
      use _ <- nibble.do(nibble.token(RParen))
      return(Multiply(x, y))
    }
      |> nibble.backtrackable
      |> nibble.map(fn(op) { Continue(operations |> list.prepend(op)) }),
    // Do -> Enable
    {
      use _ <- nibble.do(nibble.token(Do))
      use _ <- nibble.do(nibble.token(LParen))
      use _ <- nibble.do(nibble.token(RParen))
      return(Enable)
    }
      |> nibble.backtrackable
      |> nibble.map(fn(op) { Continue(operations |> list.prepend(op)) }),
    // Dont -> Disable
    {
      use _ <- nibble.do(nibble.token(Dont))
      use _ <- nibble.do(nibble.token(LParen))
      use _ <- nibble.do(nibble.token(RParen))
      return(Disable)
    }
      |> nibble.backtrackable
      |> nibble.map(fn(op) { Continue(operations |> list.prepend(op)) }),
    // if not, consume everything until we find another Mul, Do, or Dont
    nibble.take_until1("", fn(tok) { tok == Mul || tok == Do || tok == Dont })
      |> nibble.replace(operations)
      |> nibble.map(Continue),
    // if we had a Mul but the Mul parser failed, we skip
    nibble.token(Mul)
      |> nibble.replace(operations)
      |> nibble.map(Continue),
    // if we had a Do but the Do parser failed, we skip
    nibble.token(Do)
      |> nibble.replace(operations)
      |> nibble.map(Continue),
    // if we had a Dont but the Dont parser failed, we skip
    nibble.token(Dont)
      |> nibble.replace(operations)
      |> nibble.map(Continue),
    // finally, handle eof
    nibble.eof()
      |> nibble.map(fn(_) { Break(operations |> list.reverse) }),
  ])
}

pub fn pt_1(input: String) -> Int {
  let assert Ok(tokens) = lexer.run(input, lexer())
  let assert Ok(operations) = nibble.run(tokens, parser())
  use sum, op <- list.fold(operations, 0)
  case op {
    Multiply(x, y) ->
      case
        int.to_string(x) |> string.length <= 3
        && int.to_string(y) |> string.length <= 3
      {
        True -> sum + x * y
        False -> sum
      }
    _ -> sum
  }
}

pub fn pt_2(input: String) {
  let assert Ok(tokens) = lexer.run(input, lexer())
  let assert Ok(operations) = nibble.run(tokens, parser())
  list.fold(operations, #(Enable, 0), fn(tup, op) {
    let #(status, sum) = tup
    case op {
      Multiply(x, y) ->
        case
          int.to_string(x) |> string.length <= 3
          && int.to_string(y) |> string.length <= 3
          && status == Enable
        {
          True -> #(status, sum + x * y)
          False -> tup
        }
      Enable -> #(Enable, sum)
      Disable -> #(Disable, sum)
    }
  }).1
}

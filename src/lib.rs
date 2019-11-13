// Based on this tutorial on parsers in rust -> https://bodil.lol/parser-pairs/

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

pub trait Parser<'a, Output> {
  fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

// Implement Parser trait for functions with signature as follows
impl<'a, F, Output> Parser<'a, Output> for F
where
  F: Fn(&'a str) -> ParseResult<Output>,
{
  fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
    self(input)
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
  name: String,
  attributes: Vec<(String, String)>,
  children: Vec<Element>,
}

// Match literal to parse a single char
pub fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
  // Create closure that takes input str slice
  move |input: &'a str| match input.get(0..expected.len()) {
    // If found char as next return remaining str slice
    Some(next) if next == expected => Ok((&input[expected.len()..], ())),
    _ => Err(input),
  }
}

#[test]
fn literal_parser() {
  let parse_joe = match_literal("A");
  assert_eq!(Ok(("", ())), parse_joe.parse("A"));
  assert_eq!(Ok((" B", ())), parse_joe.parse("A B"));
  assert_eq!(Err("B"), parse_joe.parse("B"));
}

// Identifier to create a parser based on an input
pub fn identifier(input: &str) -> ParseResult<String> {
  let mut matched = String::new();
  let mut chars = input.chars();
  // Ensure identifier starts with alpha
  match chars.next() {
    Some(next) if next.is_alphabetic() => matched.push(next),
    _ => return Err(input),
  }
  // continue until non-alpha or -
  while let Some(next) = chars.next() {
    if next.is_alphanumeric() || next == '-' {
      matched.push(next);
    } else {
      break;
    }
  }
  // Pass on remaining str and matched string identifier
  let next_index = matched.len();
  Ok((&input[next_index..], matched))
}

#[test]
fn identifier_parser() {
  assert_eq!(
    Ok(("", "identifier-a".to_string())),
    identifier("identifier-a")
  );
  assert_eq!(
    Ok((" identifier", "partially".to_string())),
    identifier("partially identifier")
  );
  assert_eq!(Err("!not identifier"), identifier("!not identifier"));
}

// Combinator for creating custom parsers from two others
pub fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
  P1: Parser<'a, R1>,
  P2: Parser<'a, R2>,
{
  // Create a move closure that takes an input string (is a parser)
  move |input| {
    // It calls the parse function impl on the first parser
    parser1.parse(input).and_then(|(next_input, result1)| {
      // If Ok call call the second parsers parse function
      parser2
        .parse(next_input)
        // Map the output tuple to the required return type
        .map(|(last_input, result2)| (last_input, (result1, result2)))
    })
  }
}

#[test]
fn pair_parser() {
  let tag_opener = pair(match_literal("<"), identifier);
  assert_eq!(
    Ok(("/>", ((), "my-first-element".to_string()))),
    tag_opener.parse("<my-first-element/>")
  );
  assert_eq!(Err("oops"), tag_opener.parse("oops"));
  assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
}

// Map pair to modify results
pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
  P: Parser<'a, A>,
  F: Fn(A) -> B,
{
  move |input| {
    parser
      .parse(input)
      .map(|(next_input, result)| (next_input, map_fn(result)))
  }
}

// Left functor that allows us to create a function out of an input with a modified return value, of the left of a tuple
pub fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
  P1: Parser<'a, R1>,
  P2: Parser<'a, R2>,
{
  map(pair(parser1, parser2), |(left, _right)| left)
}

// Right functor that allows us to create a function out of an input with a modified return value of the right of a tuple
pub fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
  P1: Parser<'a, R1>,
  P2: Parser<'a, R2>,
{
  map(pair(parser1, parser2), |(_left, right)| right)
}

#[test]
fn right_combinator() {
  let tag_opener = right(match_literal("<"), identifier);
  assert_eq!(
    Ok(("/>", "my-first-element".to_string())),
    tag_opener.parse("<my-first-element/>")
  );
  assert_eq!(Err("oops"), tag_opener.parse("oops"));
  assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
}

pub fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
  P: Parser<'a, A>,
{
  move |mut input| {
    let mut result = Vec::new();

    if let Ok((next_input, first_item)) = parser.parse(input) {
      input = next_input;
      result.push(first_item);
    } else {
      return Err(input);
    }

    while let Ok((next_input, next_item)) = parser.parse(input) {
      input = next_input;
      result.push(next_item);
    }

    Ok((input, result))
  }
}

#[test]
fn one_or_more_combinator() {
  let parser = one_or_more(match_literal("ha"));
  assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
  assert_eq!(Err("ahah"), parser.parse("ahah"));
  assert_eq!(Err(""), parser.parse(""));
}

pub fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
  P: Parser<'a, A>,
{
  move |mut input| {
    let mut result = Vec::new();

    while let Ok((next_input, next_item)) = parser.parse(input) {
      input = next_input;
      result.push(next_item);
    }

    Ok((input, result))
  }
}

#[test]
fn zero_or_more_combinator() {
  let parser = zero_or_more(match_literal("ha"));
  assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
  assert_eq!(Ok(("ahah", vec![])), parser.parse("ahah"));
  assert_eq!(Ok(("", vec![])), parser.parse(""));
}

pub fn n_or_more<'a, P, A>(parser: P, bound: i32) -> impl Parser<'a, Vec<A>>
where
  P: Parser<'a, A>,
{
  move |mut input| {
    let mut result = Vec::new();
    let mut count = 0;
    while count < bound {
      if let Ok((next_input, first_item)) = parser.parse(input) {
        input = next_input;
        result.push(first_item);
        count += 1;
      } else {
        return Err(input);
      }
    }

    while let Ok((next_input, next_item)) = parser.parse(input) {
      input = next_input;
      result.push(next_item);
    }

    Ok((input, result))
  }
}

#[test]
fn n_or_more_combinator() {
  let parser_of_2 = n_or_more(match_literal("ha"), 2);
  assert_eq!(Ok(("", vec![(), (), ()])), parser_of_2.parse("hahaha"));
  assert_eq!(Ok((" wow", vec![(), ()])), parser_of_2.parse("haha wow"));
  assert_eq!(Err("ahah"), parser_of_2.parse("ahah"));
  assert_eq!(Err(""), parser_of_2.parse(""));

  let parser_of_0 = n_or_more(match_literal("ha"), 0);
  assert_eq!(Ok(("", vec![(), (), ()])), parser_of_0.parse("hahaha"));
  assert_eq!(Ok(("ahah", vec![])), parser_of_0.parse("ahah"));
  assert_eq!(Ok(("", vec![])), parser_of_0.parse(""));
}

// My attempts to try and come up with something for a range based combinator
// pub fn range_combinator<'a, P, A, R, N>(parser: P, range: R) -> impl Parser<'a, Vec<A>>
// where
//   P: Parser<'a, A>,
//   R: RangeBounds<N>,
// {
//   let mut count = 0;
//   let start = range.start_bound();
//   let end = range.end_bound();

//   move |mut input| {
//     let mut result = Vec::new();

//     while count < start {
//       if let Ok((next_input, first_item)) = parser.parse(input) {
//         input = next_input;
//         result.push(first_item);
//       } else {
//         return Err(input);
//       }
//     }

//     while let Ok((next_input, next_item)) = parser.parse(input) {
//       input = next_input;
//       result.push(next_item);
//     }

//     Ok((input, result))
//   }
// }

pub fn any_char(input: &str) -> ParseResult<char> {
  match input.chars().next() {
    Some(next) => Ok((&input[next.len_utf8()..], next)),
    _ => Err(input),
  }
}

pub fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
  P: Parser<'a, A>,
  F: Fn(&A) -> bool,
{
  move |input| {
    if let Ok((next_input, value)) = parser.parse(input) {
      if predicate(&value) {
        return Ok((next_input, value));
      }
    }
    Err(input)
  }
}

#[test]
fn predicate_combinator() {
  let parser = pred(any_char, |c| *c == 'o');
  assert_eq!(Ok(("mg", 'o')), parser.parse("omg"));
  assert_eq!(Err("lol"), parser.parse("lol"));
}

pub fn whitespace_char<'a>() -> impl Parser<'a, char> {
  pred(any_char, |c| c.is_whitespace())
}

pub fn one_or_more_whitespace<'a>() -> impl Parser<'a, Vec<char>> {
  one_or_more(whitespace_char())
}

pub fn zero_or_more_whitespace<'a>() -> impl Parser<'a, Vec<char>> {
  zero_or_more(whitespace_char())
}

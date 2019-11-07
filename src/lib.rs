// Based on this tutorial on parsers in rust -> https://bodil.lol/parser-combinators/

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
  name: String,
  attributes: Vec<(String, String)>,
  children: Vec<Element>,
}

pub fn match_literal(expected: &'static str) -> impl Fn(&str) -> Result<(&str, ()), &str> {
  // Create closure that takes input str slice
  move |input| match input.get(0..expected.len()) {
    // If found char as next return remaining str slice
    Some(next) if next == expected => Ok((&input[expected.len()..], ())),
    _ => Err(input),
  }
}

#[test]
fn literal_parser() {
  let parse_joe = match_literal("A");
  assert_eq!(Ok(("", ())), parse_joe("A"));
  assert_eq!(Ok((" B", ())), parse_joe("A B"));
  assert_eq!(Err("B"), parse_joe("B"));
}

pub fn identifier(input: &str) -> Result<(&str, String), &str> {
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

pub fn combinator<P1, P2, R1, R2>(
  parser1: P1,
  parser2: P2,
) -> impl Fn(&str) -> Result<(&str, (R1, R2)), &str>
where
  P1: Fn(&str) -> Result<(&str, R1), &str>,
  P2: Fn(&str) -> Result<(&str, R2), &str>,
{
  // Create a closure that takes an input str slice
  move |input| match parser1(input) {
    // If okay match on parser1 parse with parser2
    Ok((next_input, result1)) => match parser2(next_input) {
      // If okay match on parser2 return both successful matches
      Ok((final_input, result2)) => Ok((final_input, (result1, result2))),
      // Return error from parser2
      Err(err) => Err(err),
    },
    // Return error from parser1
    Err(err) => Err(err),
  }
}

#[test]
fn combinator_parser() {
  let tag_opener = combinator(match_literal("<"), identifier);
  assert_eq!(
    Ok(("/>", ((), "my-first-element".to_string()))),
    tag_opener("<my-first-element/>")
  );
  assert_eq!(Err("oops"), tag_opener("oops"));
  assert_eq!(Err("!oops"), tag_opener("<!oops"));
}

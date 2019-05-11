#[derive(PartialEq, Debug, Clone)]
pub enum AstNode {
    Integer(i32),
}

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
    where F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
        where
            P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}


fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}

fn pred<'a, P, A, F>(parser: P, predicate: F)
                     -> impl Parser<'a, A>
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

fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

fn zero_or_more<'a, P, A>(parser: P)
                          -> impl Parser<'a, Vec<A>>
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

fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

fn whitespace_wrap<'a, P, A>(parser: P)
                             -> impl Parser<'a, A>
    where
        P: Parser<'a, A>,
{
    right(space0(), left(parser, space0()))
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2)
                            -> impl Parser<'a, R1>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F)
                       -> impl Parser<'a, B>
    where
        P: Parser<'a, A>,
        F: Fn(A) -> B,
{
    move |input|
        parser.parse(input)
            .map(|(next_input, result)|
                (next_input, map_fn(result)))
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2)
                            -> impl Parser<'a, (R1, R2)>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2.parse(next_input).map(|(last_input, result2)|
                (last_input, (result1, result2)))
        })
    }
}


fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2)
                             -> impl Parser<'a, R2>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

fn number(input : &str) -> Result<(&str, AstNode), &str> {
    match input.chars().next() {
        Some(n)
            if n.is_numeric() =>
                Ok((&input[n.len_utf8()..],
                    AstNode::Integer(n.to_digit(10).unwrap() as i32))),
        _ => Err(input),
    }
}

fn expr<'a>(input: &str) -> impl Parser<'a, AstNode> {
    whitespace_wrap(number)
}

pub fn parse(source: &str) -> Result<Vec<AstNode>, String> {
    println!("{:?}", number(source));
    Ok(vec![])
}

/***********************************************************************/
/* PARSER TESTS                                                        */
/***********************************************************************/

#[test]
fn parse_single_number() {
    let parser = number;
    assert_eq!(parser.parse("1"),
               Ok(("", AstNode::Integer(1))));
    assert_eq!(parser.parse("2"),
               Ok(("", AstNode::Integer(2))));
}

#[test]
fn parse_whitespace_number() {
   let parser = whitespace_wrap(number);
    assert_eq!(parser.parse(" 1 "),
               Ok(("", AstNode::Integer(1))));
    assert_eq!(parser.parse("      2 "),
               Ok(("", AstNode::Integer(2))));
    assert_eq!(parser.parse("  3               "),
               Ok(("", AstNode::Integer(3))));
}
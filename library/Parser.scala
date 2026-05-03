package pwd4llm

/** Parsers may be in an accepting, pending or failed state.
  */
enum ParserState {

  /** Parser accepts currently. */
  case Accepting

  /** Parser rejects the correlating input, but it may still be a valid prefix.
    */
  case Pending

  /** Parser rejects the correlating input, because it is an invalid prefix. All
    * the children produced by Parser.feed must also be in the failed state.
    */
  case Failed
}

import ParserState.*

/** Basic trait that denotes what a parser is.
  * @tparam T
  *   is the token type
  * @tparam R
  *   is the type of the result
  */
trait Parser[T, R] {
  def feed(t: T): Parser[T, R]
  def result: R
  def state: ParserState
}

/** Allows to use the Parser type from any DeriviativeParsers trait
  */
trait DerivativeParserTools[P <: fcd.DerivativeParsers](val parsers: P) {
  class WrappedParser[R](
      inner: parsers.Parser[R]
  ) extends Parser[parsers.Elem, Iterable[R]] {
    def feed(t: parsers.Elem) = WrappedParser(inner.consume(t))
    def result = inner.results
    def state =
      if inner.accepts then Accepting
      else if inner.failed then Failed
      else Pending
  }

  /** Alias for parsers.Parser, so it is not confused with pwd4llm.Parser
    */
  type DParser[R] = parsers.Parser[R]
}

/** The tools for the DerivativeParsers object
  */
object DerivativeParsersTools
    extends DerivativeParserTools(fcd.DerivativeParsers)

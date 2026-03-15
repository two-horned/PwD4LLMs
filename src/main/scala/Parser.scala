package pwd4llm

/** Parsers may be in an accepting, rejecting or pending state.
  */
enum ParserStatus {

  /** Parser accepts currently. */
  case Accepting

  /** Parser rejects currently. All the children produced by Parser.feed must
    * also be in a rejecting state.
    */
  case Rejecting

  /** Parser neither accepts nor rejects. So, currently, the input correlating
    * to this parser is not in the language, but may still be a valid prefix.
    */
  case Pending
}

import ParserStatus.*

/** Basic trait that denotes what a parser is.
  * @tparameter
  *   T is the token type
  * @tparameter
  *   R is the type of the results
  */
trait Parser[T, R] {
  def feed(t: T): Parser[T, R]
  def results: R
  def status: ParserStatus
}

/** Allows to use the Parser type from any DeriviativeParsers trait
  */
trait DerivativeParserTools[P <: fcd.DerivativeParsers](val parsers: P) {
  class WrappedParser[R](
      inner: parsers.Parser[R]
  ) extends Parser[parsers.Elem, parsers.Results[R]] {
    def feed(t: parsers.Elem) = WrappedParser(inner.consume(t))
    def results = inner.results
    def status =
      if inner.accepts then Accepting
      else if inner.failed then Rejecting
      else Pending
  }
}

/** The tools for the DerivativeParsers object
  */
object DerivativeParsersTools
    extends DerivativeParserTools(fcd.DerivativeParsers)

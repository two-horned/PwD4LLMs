package pwd4llm

enum ParserStatus {
  case Accepting
  case Rejecting
  case Pending
}

import ParserStatus.*

trait BacktrackingParser[T, R] {
  def feed(t: T): BacktrackingParser[T, R]
  def unfeed: Option[BacktrackingParser[T, R]]
  def unfeedAll: BacktrackingParser[T, R]
  def results: R
  def status: ParserStatus
}

trait BacktrackingTools[P <: fcd.DerivativeParsers](val parsers: P) {
  class WrappedParser[R](
      inner: parsers.Parser[R],
      predecessor: Option[WrappedParser[R]] = None
  ) extends BacktrackingParser[parsers.Elem, parsers.Results[R]] {
    def feed(t: parsers.Elem) = WrappedParser(inner.consume(t), Some(this))
    def unfeed = predecessor
    def unfeedAll = predecessor match {
      case Some(p) => p.unfeedAll; case _ => this
    }
    def results = inner.results
    def status =
      if inner.accepts then Accepting
      else if inner.failed then Rejecting
      else Pending
  }
}

// Examples

object PythonParsersTools extends BacktrackingTools(fcd.PythonParsers)
//object DerivativeParsersTools extends BacktrackingTools(fcd.DerivativeParsers)

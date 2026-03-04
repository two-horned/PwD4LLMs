package pwd4llm

enum ParseResult[R] {
  case Success(result: R)
  case Failure(partial_result: R)
  case CriticalFailure()
}

import ParseResult.*
import pwd4llm.ParserStatus.*
import pwd4llm.GeneratorAction.*

def passOneParse[R, T, G <: TokenGenerator[T]](
    p: BacktrackingParser[T, R],
    g: G
): ParseResult[R] = {

  def go(p: BacktrackingParser[T, R]): ParseResult[R] = {
    g.receiveFeedback(p.status)
    val a = g.suggest()

    a match {
      case Reset()   => go(p.unfeedAll)
      case Append(t) => go(p.feed(t))
      case DeleteLast() =>
        p.unfeed match {
          case Some(x) => go(x)
          case None    => CriticalFailure()
        }
      case Finish() =>
        p.status match {
          case Accepting => Success(p.results)
          case _         => Failure(p.results)
        }
    }
  }
  go(p)
}

@main def main() = {
  val g = new RandoPythonTokenGen

  import PythonParsersTools.*

  val p = WrappedParser(parsers.preprocess(parsers.file_input))
  println(p)
  println(g)
  println("Let's go! One parse pass...")
  println(passOneParse(p, g))
  println(g.currentTokens)
}

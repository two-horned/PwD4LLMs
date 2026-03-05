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
    //println("next action is " + a)

    a match {
      case Concatenate(ts) => go(p.feedAll(ts))
      case Append(t)       => go(p.feed(t))
      case DeleteLast() =>
        p.unfeed match {
          case Some(x) => go(x)
          case None    => CriticalFailure()
        }
      case ReplaceLast(t) =>
        p.unfeed match {
          case Some(x) => go(x.feed(t))
          case None    => CriticalFailure()
        }
      case Rebuild(ts) => go(p.unfeedAll.feedAll(ts))
      case Reset() => go(p.unfeedAll)
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

  import PythonParsersTools.*
  import fcd.PythonParsers.*


  // {
  //   println("Let's go! One parse pass with Rando TG")
  //   val g = new RandoPythonTokenGen
  //   println(passOneParse(p, g))
  // }

  {
    val p = WrappedParser(and(atMost(5, acceptIf(_ => true)),
        parsers.preprocess(parsers.file_input)))
    println("Let's go! One parse pass with DFS TG")
    val g = new DFS_PythonTG
    println(passOneParse(p, g))
  }

  {
    val p = WrappedParser(and(atMost(10, acceptIf(_ => true)),
        parsers.preprocess(parsers.file_input)))
    println("Let's go! One parse pass with BFS TG")
    val g = new BFS_PythonTG
    println(passOneParse(p, g))
  }
}

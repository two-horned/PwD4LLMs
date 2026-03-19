import pwd4llm.*
import examples.*
import PCF.*
import fcd.DerivativeParsers.*
import DerivativeParsersTools.*
import EvalResult.*

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.language.implicitConversions

val logger = Logger(LoggerFactory.getLogger(this.getClass.getSimpleName))

def useParserOutput(output: List[(String, Expr)]) = {
  require(output.length == 1) // grammar is deterministic

  for case (input, expr) <- output do {
    logger.info("Input was: " + input)
    logger.info("Resulting expression is: " + expr)
    logger.info("Use good input for markov chain...")
    updateMarkovChain(input)
  }
}

case class EvaluationCommand(least: Int, most: Int, iterations: Int)

@main def main() = {
  val commands: List[EvaluationCommand] = List(
    EvaluationCommand(0, 5, 10),
    EvaluationCommand(5, 10, 1000),
    EvaluationCommand(10, 20, 100000),
    // EvaluationCommand(20, 30, 10000),
  )

  time {
    import StackEvaluator.eval
    for command <- commands do {
      for _ <- 0 until command.iterations do {

        val at_least_at_most: fcd.DerivativeParsers.Parser[String] =
          not(atMost(command.least, any)) &> atMost(command.most, any)
        val p = WrappedParser(at_least_at_most & strict_expr)
        val g = new DFS_PCF_TG
        eval(p, g) match {
          case Success(r) => useParserOutput(r)
          case _          => logger.warn("One evaluation failed")
        }
      }
    }
  }

  // time {
  //   import StackEvaluator.eval
  //   println("Let's go! One parse pass with BFS TG")
  //   val g = new BFS_PCF_TG
  //   println(eval(p, g))
  // }

  // time {
  //   import ScrapAllEvaluator.eval
  //   println("Let's go! One parse pass with BFS TG, scrapping all")
  //   val g = new BFS_PCF_TG
  //   println(eval(p, g))
  // }
}

def time[T](block: => T): T = {
  val before = System.nanoTime
  val result = block
  val difference = System.nanoTime - before
  if difference > 1000 then
    println("Elapsed time: " + difference / 1000000 + "ms")
  else println("Elapsed time: " + difference / 1000 + "µs")
  result
}

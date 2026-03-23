import pwd4llm.*
import example.pcf.*
import util.verbosify
import fcd.DerivativeParsers.*
import DerivativeParsersTools.*
import EvalResult.*

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.language.implicitConversions
import scala.collection.immutable.ArraySeq

val logger = Logger(LoggerFactory.getLogger(this.getClass.getSimpleName))

val repetitions = 19
val verbose_language = verbosify(strict_expr, repetitions)
def newTG(): TokenGenerator[Char] = new DFS_PCF_VERBOSE_TG(repetitions)

def useParserOutput(output: ArraySeq[(String, Expr)]) = {
  require(output.length == 1) // grammar is deterministic
  for case (input, expr) <- output do updateMarkovChain(input)
}

case class EvaluationCommand(least: Int, most: Int, iterations: Int)

@main def main() = {

  val train_commands: Array[EvaluationCommand] = Array(
    EvaluationCommand(5, 7, 5000),
    EvaluationCommand(7, 9, 1000),
    EvaluationCommand(9, 11, 100)
  )

  {
    import StackEvaluator.eval
    for command <- train_commands do {
      val at_least_at_most: fcd.DerivativeParsers.Parser[String] =
        not(atMost(command.least, any)) &> atMost(command.most, any)
      logger.info("Training on cmd {}", command);
      for _ <- 0 until command.iterations do {
        val p = WrappedParser(at_least_at_most & strict_expr)
        val g = new DFS_PCF_TG
        eval(p, g) match {
          case Success(r) => useParserOutput(ArraySeq.from(r()))
          case _          => logger.warn("One evaluation failed")
        }
      }
    }
  }

  val command =
    EvaluationCommand((1 + repetitions) * 5, (1 + repetitions) * 7, 10)
  val at_least_at_most: DParser[String] =
    not(atMost(command.least, any)) &> atMost(command.most, any)
  val evaluators =
    Array(StackEvaluator, ScrapAllEvaluator, RememberActionEvaluator)
  for e <- evaluators do {
    logger.info("Time of \"pretrained\" {} and cmd {}",
      e.getClass.getSimpleName, command);
    time {
      import e.eval
      for _ <- 0 until command.iterations do {
        val p = WrappedParser(at_least_at_most & verbose_language)
        eval(p, newTG()) match {
          case Success(r) => logger.info("Evaluation succeeded with {}.", r())
          case Failure(r) => logger.warn("One evaluation failed with {}.", r())
          case CriticalFailure() =>
            logger.warn("One evaluation critically failed")
        }
      }
    }
  }

  resetMarkovChain()

  for e <- evaluators do {
    logger.info("Time of \"untrained\" {} and cmd {}", e.getClass.getSimpleName,
      command);
    time {
      import e.eval
      for _ <- 0 until command.iterations do {
        val p = WrappedParser(at_least_at_most & verbose_language)
        eval(p, newTG()) match {
          case Success(r) => logger.info("Evaluation succeeded with {}.", r())
          case Failure(r) => logger.warn("One evaluation failed with {}.", r())
          case CriticalFailure() =>
            logger.warn("One evaluation critically failed")
        }
      }
    }
  }
}

def time[T](block: => T): T = {
  val before = System.nanoTime
  val result = block
  val difference = System.nanoTime - before
  if difference > 1000 then
    logger.info("Elapsed time: " + difference / 1000000 + "ms")
  else logger.info("Elapsed time: " + difference / 1000 + "µs")
  result
}

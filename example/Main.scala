package pwd4llm.example

import pcf.*

import pwd4llm.*
import util.verbosify
import DerivativeParsersTools.*
import EvalResult.*

import fcd.DerivativeParsers.*

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import scala.language.implicitConversions

val logger = Logger(LoggerFactory.getLogger(this.getClass.getSimpleName))

val repetitions = 19
val language = strict_expr
val verbose_language = verbosify(language, repetitions)
var markov_chain = preparedMarkovChain

def newTG(): TokenGenerator[Char] =
  new DFS_PCF_TG(markov_chain)

def newVerboseTG(): TokenGenerator[Char] =
  new DFS_PCF_VERBOSE_TG(repetitions, markov_chain)

def useParserOutput(output: List[(String, Expr)]) = {
  require(output.length == 1) // grammar is deterministic
  for case (input, expr) <- output do updateMarkovChain(markov_chain, input)
}

def atLeastAtMost(least: Int, most: Int): DParser[String] =
  not(atMost(least, any)) &> atMost(most, any)

case class EvaluationCommand(least: Int, most: Int, iterations: Int)

@main def main() = {
  val train_commands: Array[EvaluationCommand] = Array(
    EvaluationCommand(5, 7, 5000),
    EvaluationCommand(7, 9, 1000),
    EvaluationCommand(9, 11, 100)
  )

  {
    import StackEvaluator.eval
    for cmd <- train_commands do {
      logger.info("Training on cmd {}", cmd);
      for _ <- 0 until cmd.iterations do {
        val p = WrappedParser(atLeastAtMost(cmd.least, cmd.most) & language)
        val g = newTG()
        eval(p, g) match {
          case Success(r) => useParserOutput(List.from(r()))
          case _          => logger.warn("One evaluation failed")
        }
      }
    }
  }

  val cmd = EvaluationCommand((1 + repetitions) * 5, (1 + repetitions) * 7, 10)
  val p = WrappedParser(atLeastAtMost(cmd.least, cmd.most) &> verbose_language)

  val evaluators =
    Array(StackEvaluator, ScrapAllEvaluator, RememberActionEvaluator)
  for e <- evaluators do {
    time {
      logger.info("Time of \"pretrained\" {} and cmd {}",
        e.getClass.getSimpleName, cmd);
      for _ <- 0 until cmd.iterations do {
        e.eval(p, newVerboseTG()) match {
          case Success(r) => logger.info("Evaluation succeeded with {}.", r())
          case Failure(r) => logger.warn("One evaluation failed with {}.", r())
          case CriticalFailure() =>
            logger.warn("One evaluation critically failed")
        }
      }
    }
  }

  markov_chain = preparedMarkovChain

  for e <- evaluators do {
    time {
      logger.info("Time of \"untrained\" {} and cmd {}",
        e.getClass.getSimpleName, cmd);
      for _ <- 0 until cmd.iterations do {
        e.eval(p, newVerboseTG()) match {
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

package pwd4llm.example

import pcf.*

import pwd4llm.*
import util.*
import DerivativeParsersTools.*
import EvalResult.*

import fcd.DerivativeParsers.*

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import scala.language.implicitConversions

val logger = Logger(LoggerFactory.getLogger(this.getClass.getSimpleName))

val repetitions = 19
val language = strictExpr
val verbose_language = verbosify(language, repetitions)
var markov_chain = preparedMarkovChain

def trainingTG(): TokenGenerator[Char] =
  new DFS_TG(() => markov_chain.seed())

def verboseDFSTG(): TokenGenerator[Char] =
  new DFS_VERBOSE_TG(repetitions, () => markov_chain.seed())

def verboseReATG(): TokenGenerator[Char] =
  new RetryAll_VERBOSE_TG(repetitions, () => markov_chain.seed())

def useParserOutput(output: List[(String, Expr)]) = {
  require(output.length == 1) // grammar is deterministic
  for case (input, expr) <- output do updateMarkovChain(markov_chain, input)
}

def atLeastAtMost(least: Int, most: Int): DParser[String] =
  not(atMost(least, any)) &> atMost(most, any)

case class EvaluationCommand(least: Int, most: Int, iterations: Int)

@main def main() = {
  val train_commands = Array(
    EvaluationCommand(5, 7, 16000),
    EvaluationCommand(7, 9, 8000),
    EvaluationCommand(9, 11, 4000)
  )

  {
    import StackEvaluator.eval
    for cmd <- train_commands do {
      logger.info("Training on cmd {}", cmd);
      val p = WrappedParser(atLeastAtMost(cmd.least, cmd.most) & language)
      for _ <- 0 until cmd.iterations do {
        val g = trainingTG()
        eval(p, g) match {
          case Success(r) => useParserOutput(List.from(r()))
          case _          => logger.warn("One evaluation failed")
        }
      }
    }
  }

  val cmd = EvaluationCommand((1 + repetitions) * 8, (1 + repetitions) * 10, 10)
  val p = WrappedParser(atLeastAtMost(cmd.least, cmd.most) &> verbose_language)

  val evaluators = Array(StackEvaluator)
  val token_generators =
    Array(("DFS", () => verboseDFSTG()), ("RetryAll", () => verboseReATG()))
  for {
    e <- evaluators
    (tg_name, tg) <- token_generators
  } do {
    time {
      logger.info("Time of \"pretrained\" {} with {} and cmd {}",
        e.getClass.getSimpleName, tg_name, cmd);
      for _ <- 0 until cmd.iterations do {
        e.eval(p, tg()) match {
          case Success(r) => logger.info("Evaluation succeeded with {}.", r())
          case Failure(r) => logger.warn("One evaluation failed with {}.", r())
          case CriticalFailure() =>
            logger.warn("One evaluation critically failed")
        }
      }
    }
  }

  markov_chain = preparedMarkovChain

  for {
    e <- evaluators
    (tg_name, tg) <- token_generators
  } do {
    time {
      logger.info("Time of \"untrained\" {} with {} and cmd {}",
        e.getClass.getSimpleName, tg_name, cmd);
      for _ <- 0 until cmd.iterations do {
        e.eval(p, tg()) match {
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
  val difference = (System.nanoTime - before).toDouble
  if difference > 1000000000 then
    logger.info("Elapsed time: " + difference / 1000000000 + "s")
  else if difference > 1000000 then
    logger.info("Elapsed time: " + difference / 1000000 + "ms")
  else if difference > 1000 then
    logger.info("Elapsed time: " + difference / 1000 + "µs")
  else logger.info("Elapsed time: " + difference + "ns")
  result
}

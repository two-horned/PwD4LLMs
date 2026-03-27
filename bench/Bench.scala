package bench

import pwd4llm.*
import internal.MarkovChain
import example.pcf.*
import util.verbosify
import fcd.DerivativeParsers.*
import DerivativeParsersTools.*
import EvalResult.*

import org.openjdk.jmh.annotations.*
import scala.language.implicitConversions

val repetitions = 19
val language = strict_expr
val verbose_language = verbosify(language, repetitions)

case class EvaluationCommand(least: Int, most: Int, iterations: Int)

val cmd = EvaluationCommand((1 + repetitions) * 5, (1 + repetitions) * 7, 10)
val p = WrappedParser(atLeastAtMost(cmd.least, cmd.most) & verbose_language)

def atLeastAtMost(least: Int, most: Int): DParser[String] =
  not(atMost(least, any)) &> atMost(most, any)

def newTG(markov_chain: MarkovChain[Char]): TokenGenerator[Char] =
  new DFS_PCF_TG(markov_chain)

def newVerboseTG(markov_chain: MarkovChain[Char]): TokenGenerator[Char] =
  new DFS_PCF_VERBOSE_TG(repetitions, markov_chain)

def useParserOutput(
    markov_chain: MarkovChain[Char],
    output: List[(String, Expr)]
) = {
  require(output.length == 1) // grammar is deterministic
  for case (input, expr) <- output do updateMarkovChain(markov_chain, input)
}

@BenchmarkMode(Array(Mode.AverageTime))
class Bench {

  @State(Scope.Benchmark)
  abstract class TrainState {
    var markov_chain: MarkovChain[Char] = null
    @Setup(Level.Trial)
    def prepare(): Unit
  }

  object TrainedState extends TrainState {
    def prepare(): Unit = {
      markov_chain = preparedMarkovChain
      val train_commands = Array(
        EvaluationCommand(5, 7, 16000),
        EvaluationCommand(7, 9, 8000),
        EvaluationCommand(9, 11, 4000)
      )
      import StackEvaluator.eval
      for cmd <- train_commands do {
        val p = WrappedParser(atLeastAtMost(cmd.least, cmd.most) & language)
        for _ <- 0 until cmd.iterations do {
          val g = newTG(markov_chain)
          eval(p, g) match {
            case Success(r) => useParserOutput(markov_chain, List.from(r()))
            case _          => ()
          }
        }
      }
    }
  }

  object UntrainedState extends TrainState {
    def prepare(): Unit = {
      markov_chain = preparedMarkovChain
    }
  }

  @Benchmark
  def bmStackEvaluator(s: TrainState) = {
    StackEvaluator.eval(p, newVerboseTG(s.markov_chain))
  }

  @Benchmark
  def bmScrapAllEvaluator(s: TrainState) = {
    ScrapAllEvaluator.eval(p, newVerboseTG(s.markov_chain))
  }

  @Benchmark
  def bmRememberActionEvaluator(s: TrainState) = {
    RememberActionEvaluator.eval(p, newVerboseTG(s.markov_chain))
  }

}

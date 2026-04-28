package bench

import pwd4llm.*
import internal.typoRateFromFpRate
import example.pcf.{strictExprEnter, Expr, seedWCFG}
import DerivativeParsersTools.*
import EvalResult.*

import org.openjdk.jmh.annotations.*
import scala.language.implicitConversions
import org.openjdk.jmh.infra.Blackhole
import scala.compiletime.uninitialized

def parser: Parser[Char, Expr] = WrappedParser(strictExprEnter).asInstanceOf

@BenchmarkMode(Mode.AverageTime)
@State(Scope.Benchmark)
class Bench {
  @Param(Array("100", "200", "400"))
  var token_length: Int = uninitialized

  @Param(Array("0.05", "0.1", "0.2", "0.4"))
  var fp_rate: Double = uninitialized

  def seed(): Node[Char] =
    seedWCFG(token_length, typoRateFromFpRate(fp_rate, token_length))

  @Benchmark
  def dfsEval() = {
    val tg = new DFS_TG(seed)
    StackEvaluator.eval(parser, tg)
  }

  @Benchmark
  def retryAllEval() = {
    val tg = new RetryAll_TG(seed)
    ScrapAllEvaluator.eval(parser, tg)
  }

  @Benchmark
  def giveUpEval() = {
    val tg = new GiveUp_TG(seed)
    ScrapAllEvaluator.eval(parser, tg)
  }
}

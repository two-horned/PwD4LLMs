package bench

import pwd4llm.*
import internal.typoRateFromFpRate
import example.pcf.{strictExprEnter, Expr, seedWCFG, seedWCFG_Simple}
import DerivativeParsersTools.*
import EvalResult.*

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.BenchmarkParams
import scala.compiletime.uninitialized

import java.util.concurrent.atomic.AtomicInteger

def parser: Parser[Char, Iterable[Expr]] = WrappedParser(strictExprEnter)

val rand = scala.util.Random()

@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@BenchmarkMode(Array(Mode.Throughput))
@Fork(1)
@State(Scope.Benchmark)
class Bench {
  @Param(Array("400", "800", "1600", "3200", "6400", "12800"))
  var token_length: Int = uninitialized

  @Param(Array("0.05", "0.1", "0.2", "0.4", "0.8", "0.95"))
  var fp_rate: Double = uninitialized

  private val max_steps_fix = 1000000
  def max_steps_linear = 5 * token_length
  def max_steps_quadratic = (token_length * token_length) / 8
  private val success_count = AtomicInteger(0)
  private val failure_count = AtomicInteger(0)

  def seed1(): Node[Char] =
    seedWCFG(token_length, typoRateFromFpRate(fp_rate, token_length), 1, rand)

  def seed2(): Node[Char] =
    seedWCFG(token_length, typoRateFromFpRate(fp_rate, token_length), 2, rand)

  def seed4(): Node[Char] =
    seedWCFG(token_length, typoRateFromFpRate(fp_rate, token_length), 4, rand)

  def seed1_Simple(): Node[Char] =
    seedWCFG_Simple(token_length, typoRateFromFpRate(fp_rate, token_length), 1,
      rand)

  def seed2_Simple(): Node[Char] =
    seedWCFG_Simple(token_length, typoRateFromFpRate(fp_rate, token_length), 2,
      rand)

  @Benchmark
  def dfsEvalBF2() = {
    val tg = new DFS_TG(seed2, max_steps_linear)
    StackEvaluator.eval(parser, tg) match {
      case Success(_) => success_count.incrementAndGet()
      case _          => failure_count.incrementAndGet()
    }
  }

  @Benchmark
  def dfsEvalBF2_Fix() = {
    val tg = new DFS_TG(seed2, max_steps_fix)
    StackEvaluator.eval(parser, tg) match {
      case Success(_) => success_count.incrementAndGet()
      case _          => failure_count.incrementAndGet()
    }
  }

  @Benchmark
  def dfsEvalBF2_Quad() = {
    val tg = new DFS_TG(seed2, max_steps_quadratic)
    StackEvaluator.eval(parser, tg) match {
      case Success(_) => success_count.incrementAndGet()
      case _          => failure_count.incrementAndGet()
    }
  }

  @Benchmark
  def dfsEvalBF2_Simple() = {
    val tg = new DFS_TG(seed2_Simple, max_steps_linear)
    StackEvaluator.eval(parser, tg) match {
      case Success(_) => success_count.incrementAndGet()
      case _          => failure_count.incrementAndGet()
    }
  }

  @Benchmark
  def dfsEvalBF2NoParserStack() = {
    val tg = new DFS_TG(seed2, max_steps_linear)
    ScrapAllEvaluator.eval(parser, tg) match {
      case Success(_) => success_count.incrementAndGet()
      case _          => failure_count.incrementAndGet()
    }
  }

  @Benchmark
  def dfsEvalBF4() = {
    val tg = new DFS_TG(seed4, max_steps_linear)
    StackEvaluator.eval(parser, tg) match {
      case Success(_) => success_count.incrementAndGet()
      case _          => failure_count.incrementAndGet()
    }
  }

  @Benchmark
  def retryAllEval() = {
    val tg = new RetryAll_TG(seed1)
    ScrapAllEvaluator.eval(parser, tg) match {
      case Success(_) => success_count.incrementAndGet()
      case _          => failure_count.incrementAndGet()
    }
  }

  @Benchmark
  def retryAllEval_Simple() = {
    val tg = new RetryAll_TG(seed1_Simple)
    ScrapAllEvaluator.eval(parser, tg) match {
      case Success(_) => success_count.incrementAndGet()
      case _          => failure_count.incrementAndGet()
    }
  }

  @Benchmark
  def giveUpEval() = {
    val tg = new GiveUp_TG(seed1)
    ScrapAllEvaluator.eval(parser, tg) match {
      case Success(_) => success_count.incrementAndGet()
      case _          => failure_count.incrementAndGet()
    }
  }

  @Benchmark
  def giveUpEval_Simple() = {
    val tg = new GiveUp_TG(seed1_Simple)
    ScrapAllEvaluator.eval(parser, tg) match {
      case Success(_) => success_count.incrementAndGet()
      case _          => failure_count.incrementAndGet()
    }
  }

  @TearDown(Level.Trial)
  def successRates(params: BenchmarkParams) = {
    val sc = success_count.get()
    val fc = failure_count.get()
    val tc = sc + fc
    val rate = sc.toDouble / tc.toDouble

    println(s"${params.getBenchmark}  $rate  $sc  $fc  $tc")
  }
}

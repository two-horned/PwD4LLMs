package bench

import pwd4llm.*
import internal.typoRateFromFpRate
import example.pcf.{strictExprEnter, Expr, seedWCFG}
import DerivativeParsersTools.*
import EvalResult.*

import org.openjdk.jmh.annotations.*
import scala.language.implicitConversions
import scala.compiletime.uninitialized

import java.util.concurrent.atomic.AtomicInteger

def parser: Parser[Char, Expr] = WrappedParser(strictExprEnter).asInstanceOf

val rand = scala.util.Random()

@BenchmarkMode(Array(Mode.AverageTime))
@State(Scope.Benchmark)
class Bench {
  @Param(Array("100", "200", "400"))
  var token_length: Int = uninitialized

  @Param(Array("0.05", "0.1", "0.2", "0.4"))
  var fp_rate: Double = uninitialized

  @Param(Array("2", "3", "4"))
  var bf: Int = uninitialized

  private val dfs_success_count = AtomicInteger(0)
  private val dfs_total_tests = AtomicInteger(0)
  private val ra_success_count = AtomicInteger(0)
  private val ra_total_tests = AtomicInteger(0)
  private val gu_success_count = AtomicInteger(0)
  private val gu_total_tests = AtomicInteger(0)

  def seed(): Node[Char] =
    seedWCFG(token_length, typoRateFromFpRate(fp_rate, token_length), bf, rand)

  @Benchmark
  def dfsEval() = {
    val tg = new DFS_TG(seed, token_length * 5)
    dfs_total_tests.incrementAndGet()
    StackEvaluator.eval(parser, tg) match {
      case Success(_) => dfs_success_count.incrementAndGet()
      case _          => dfs_success_count.get()
    }
  }

  @Benchmark
  def retryAllEval() = {
    val tg = new RetryAll_TG(seed)
    ra_total_tests.incrementAndGet()
    ScrapAllEvaluator.eval(parser, tg) match {
      case Success(_) => ra_success_count.incrementAndGet()
      case _          => ra_success_count.get()
    }
  }

  @Benchmark
  def giveUpEval() = {
    val tg = new GiveUp_TG(seed)
    gu_total_tests.incrementAndGet()
    ScrapAllEvaluator.eval(parser, tg) match {
      case Success(_) => gu_success_count.incrementAndGet()
      case _          => gu_success_count.get()
    }
  }

  @TearDown(Level.Trial)
  def successRates() = {
    val dfs_rate =
      dfs_success_count.doubleValue() / dfs_total_tests.doubleValue()
    val ra_rate = ra_success_count.doubleValue() / ra_total_tests.doubleValue()
    val gu_rate = gu_success_count.doubleValue() / gu_total_tests.doubleValue()
    println(s"Success rate of DFS: $dfs_rate")
    println(s"Success rate of RetryAll: $ra_rate")
    println(s"Success rate of GiveUp: $gu_rate")
  }
}

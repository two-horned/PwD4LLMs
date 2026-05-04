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

@BenchmarkMode(Array(Mode.AverageTime, Mode.Throughput))
@State(Scope.Benchmark)
class Bench {
  @Param(Array("100", "200", "400"))
  var token_length: Int = uninitialized

  @Param(Array("0.05", "0.1", "0.2", "0.4"))
  var fp_rate: Double = uninitialized

  private val success_count = AtomicInteger(0)
  private val failure_count = AtomicInteger(0)

  def seed1(): Node[Char] =
    seedWCFG(token_length, typoRateFromFpRate(fp_rate, token_length), 1, rand)

  def seed2(): Node[Char] =
    seedWCFG(token_length, typoRateFromFpRate(fp_rate, token_length), 2, rand)

  def seed3(): Node[Char] =
    seedWCFG(token_length, typoRateFromFpRate(fp_rate, token_length), 3, rand)

  def seed4(): Node[Char] =
    seedWCFG(token_length, typoRateFromFpRate(fp_rate, token_length), 4, rand)

  @Benchmark
  def dfsEvalBF2NoParserStack() = {
    val tg = new DFS_TG(seed2, token_length * 5)
    ScrapAllEvaluator.eval(parser, tg) match {
      case Success(_) => success_count.incrementAndGet()
      case _          => failure_count.incrementAndGet()
    }
  }

  @Benchmark
  def dfsEvalBF2() = {
    val tg = new DFS_TG(seed2, token_length * 5)
    StackEvaluator.eval(parser, tg) match {
      case Success(_) => success_count.incrementAndGet()
      case _          => failure_count.incrementAndGet()
    }
  }

  @Benchmark
  def dfsEvalBF3() = {
    val tg = new DFS_TG(seed3, token_length * 5)
    StackEvaluator.eval(parser, tg) match {
      case Success(_) => success_count.incrementAndGet()
      case _          => failure_count.incrementAndGet()
    }
  }

  @Benchmark
  def dfsEvalBF4() = {
    val tg = new DFS_TG(seed4, token_length * 5)
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
  def giveUpEval() = {
    val tg = new GiveUp_TG(seed1)
    ScrapAllEvaluator.eval(parser, tg) match {
      case Success(_) => success_count.incrementAndGet()
      case _          => failure_count.incrementAndGet()
    }
  }

  @TearDown(Level.Trial)
  def successRates() = {
    val sc = success_count.get()
    val fc = failure_count.get()
    val tc = sc + fc
    val rate = sc.toDouble / tc.toDouble

    println(s"------------------------------")
    println(s"Success rate: $rate")
    println(s"Success count: $sc")
    println(s"Failure count: $fc")
    println(s"Total count: $tc")
    println(s"------------------------------")
  }
}

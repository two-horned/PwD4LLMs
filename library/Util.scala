package pwd4llm.util

import pwd4llm.*
import GeneratorAction.*
import ParserState.*
import fcd.DerivativeParsers.*
import DerivativeParsersTools.DParser

/** Verbosify a language by requiring each symbol to be repeated a number of
  * times. For example, {ab, bba, cd} becomes {aaabbb, bbbbbbaaa, cccddd} when
  * the number of repetitions is 2.
  *
  * @param language
  * @param repetitions
  * @return
  */
def verbosify[T](language: DParser[T], repetitions: Int): DParser[T] = {
  require(repetitions > 0)
  def go[T](p: DParser[T], count: Int = 0, char: Char = 0): DParser[T] = {
    if count == 0 then return done(p) | eat(c => go(p << c, repetitions, c))
    eat(c => go(p, count - 1, char))
  }

  go(language)
}

final class DFS_VERBOSE_TG(
    repetitions: Int,
    seed: () => Node[Char]
) extends TokenGenerator[Char] {
  private val inner = new DFS_TG(seed)

  def suggest(): GeneratorAction[Char] = {
    inner.suggest() match {
      case Append(token) => Concatenate(Iterator.fill(1 + repetitions)(token))
      case DeleteLast()  => DropLast(1 + repetitions)
      case x @ Finish()  => x
      case _             => ???
    }
  }

  def receiveFeedback(state: ParserState) = inner.receiveFeedback(state)
}

final class RetryAll_VERBOSE_TG(
    repetitions: Int,
    seed: () => Node[Char]
) extends TokenGenerator[Char] {
  private val inner = new RetryAll_TG(seed)

  def suggest(): GeneratorAction[Char] = {
    inner.suggest() match {
      case Append(token) => Concatenate(Iterator.fill(1 + repetitions)(token))
      case x @ (Finish() | Reset()) => x
      case _                        => ???
    }
  }

  def receiveFeedback(state: ParserState) = inner.receiveFeedback(state)
}

final class GiveUp_VERBOSE_TG(
    repetitions: Int,
    seed: () => Node[Char]
) extends TokenGenerator[Char] {
  private val inner = new GiveUp_TG(seed)

  def suggest(): GeneratorAction[Char] = {
    inner.suggest() match {
      case Append(token) => Concatenate(Iterator.fill(1 + repetitions)(token))
      case x @ Finish()  => x
      case _             => ???
    }
  }

  def receiveFeedback(state: ParserState) = inner.receiveFeedback(state)
}

package pwd4llm.util

import pwd4llm.DParser
import fcd.DerivativeParsers.*

/** Verbosify a language by requiring each symbol to be repeated a number of times.
  * For example, {ab, bba, cd} becomes  {aaabbb, bbbbbbaaa, cccddd} when the repition is 2.
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

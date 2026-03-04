package pwd4llm

import scala.collection.mutable.Stack

enum GeneratorAction[Token] {
  case Append(token: Token)
  case DeleteLast()
  case Finish()
  case Reset()
}

import GeneratorAction.*
import pwd4llm.ParserStatus.*

trait TokenGenerator[Token] {
  def suggest(): GeneratorAction[Token]
  def receiveFeedback(status: ParserStatus): Unit
}

// Examples

abstract class DFS_TG[T] extends TokenGenerator[T] {
  private val levels: Stack[Iterator[T]] = Stack()
  private var backtrack = false
  levels.push(seed)

  def newLevel(): Iterator[T]
  def seed: Iterator[T]

  final def suggest() = {
    if levels.isEmpty then return Finish()

    if backtrack then {
      backtrack = false
      levels.pop()
      return DeleteLast()
    }

    val current = levels.top
    current.nextOption() match {
      case Some(x) => {
        levels.push(newLevel())
        Append(x)
      }
      case _ => {
        levels.pop()
        DeleteLast()
      }
    }
  }

  final def receiveFeedback(status: ParserStatus) = status match {
      case Accepting => levels.clear()
      case Rejecting => backtrack = true
      case _ => ()
  }
}

class DFS_PythonTG extends DFS_TG[fcd.PythonParsers.Elem] {
  import fcd.PythonParsers.Lexeme.*
  import scala.util.Random

  private val random = new Random

  private var token_list =
    Array(Id("xyz"), WS, NL, Punct("="), Punct("+"), Punct("*"), EOS)

  def finishPredicate() = true
  def newLevel() = random.shuffle(token_list).iterator
  def seed: Iterator[fcd.PythonParsers.Elem] = Some(Id("xyz")).iterator
}


class RandoPythonTokenGen extends TokenGenerator[fcd.PythonParsers.Elem] {
  import fcd.PythonParsers.Lexeme.*
  import scala.util.Random

  private val MAX_TOKENS = 30
  private val BIAS = 10

  private var random = new Random
  private var token_length = 0
  private var last_status = Pending

  private var token_list =
    Array(Id("xyz"), WS, NL, Punct("="), Punct("+"), Punct("*"), EOS)

  def suggest(): GeneratorAction[fcd.PythonParsers.Lexeme] = {
    last_status match {
      case Rejecting => {
        if 0 == random.nextInt(
            MAX_TOKENS - scala.math.min(MAX_TOKENS, token_length)
          )
        then {
          token_length = 0
          Reset()
        } else {
          token_length -= 1
          DeleteLast()
        }
      }
      case Accepting => {
        if 0 == random.nextInt(
            MAX_TOKENS - scala.math.min(MAX_TOKENS - BIAS, token_length)
          )
        then Finish()
        else {
          token_length = 0
          Reset()
        }
      }
      case Pending => {
        if 0 == random.nextInt(
            MAX_TOKENS - scala.math.min(MAX_TOKENS, token_length)
          )
        then {
          token_length = 0
          Reset()
        } else {
          token_length += 1
          val token = token_list(random.nextInt(token_list.length))
          Append(token)
        }
      }
    }
  }

  def receiveFeedback(status: ParserStatus): Unit = {
    last_status = status
  }
}

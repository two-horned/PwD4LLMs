package pwd4llm

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

import scala.collection.mutable.{Stack, Queue}

case class Node[T](neighbors: Iterator[(T, () => Node[T])])

abstract class DFS_TG[T] extends TokenGenerator[T] {
  private val levels: Stack[Node[T]] = Stack()
  private var backtrack = false
  levels.push(seed)

  def seed: Node[T]

  final def suggest() = {
    if levels.isEmpty then return Finish()

    if backtrack then {
      backtrack = false
      levels.pop()
      return DeleteLast()
    }

    levels.top.neighbors.nextOption() match {
      case Some((token, nodeMaker)) => {
        levels.push(nodeMaker())
        Append(token)
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
    case _         => ()
  }
}

/*
abstract class BFS_TG[T] extends TokenGenerator[T] {
  private val levels: Queue[Node[T]] = Queue()
  private var backtrack = false
  levels.enqueue(seed)

  def seed: Node[T]

  final def suggest() = {
    if levels.isEmpty then return Finish()

    if backtrack then {
      backtrack = false
      levels.dequeue()
      return DeleteLast()
    }

    levels.last.neighbors.nextOption() match {
      case Some((token, node)) => {
        levels.enqueue(node)
        Append(token)
      }
      case _ => {
        levels.dequeue()
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
 */

class DFS_PythonTG extends DFS_TG[fcd.PythonParsers.Elem] {
  import fcd.PythonParsers.Lexeme
  import fcd.PythonParsers.Lexeme.*
  import scala.util.Random

  private val random = new Random()

  private def token_list =
    Array(Id("xyz"), WS, NL, Punct("="), Punct("+"), Punct("*"), EOS)

  private def node(): Node[fcd.PythonParsers.Elem] =
    Node(random.shuffle(token_list).view.map(x => (x, () => node())).iterator)

  def seed: Node[fcd.PythonParsers.Elem] =
    Node(Iterator((Id("xyz"), () => node())))
}

/*
class BFS_PythonTG extends BFS_TG[fcd.PythonParsers.Elem] {
  import fcd.PythonParsers.Lexeme
  import fcd.PythonParsers.Lexeme.*
  import scala.util.Random
  import scala.collection.immutable.LazyList
  import scala.collection.immutable.LazyList.*

  private val random = new Random
  private def token_list() = random.shuffle(Array(Id("xyz"), WS, NL, Punct("="), Punct("+"), Punct("*"), EOS))

  class PythonNode extends Node[Lexeme] {
    private val shuffled_tokens = token_list()
    def neighbors = shuffled_tokens.view.map(x => (x, new PythonNode)).iterator
  }

  def seed: Node[fcd.PythonParsers.Elem] = new PythonNode
 }
 */

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

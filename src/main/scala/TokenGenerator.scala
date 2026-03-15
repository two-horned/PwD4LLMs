package pwd4llm

/** Defines the actions TokenGenerators can request.
  *
  * @tparameter
  *   T is type of the tokens the TokenGenerator generates
  */
enum GeneratorAction[Token] {
  case Concatenate(tokens: Iterator[Token])
  case Append(token: Token)
  case DropLast(number: Int)
  case DeleteLast()
  case ReplaceLast(token: Token)
  case Rebuild(tokens: Iterator[Token])
  case Finish()
  case Reset()
}

import GeneratorAction.*

/** Basic trait that denotes what a token generator is.
  *
  * @tparameter
  *   T is the type of the tokens it may generate
  */
import pwd4llm.ParserStatus.*

trait TokenGenerator[T] {
  def suggest(): GeneratorAction[T]
  def receiveFeedback(status: ParserStatus): Unit
}

/** An abstract node or vertice in a search tree that connects to other nodes or
  * vertices via tokens.
  *
  * @tparameter
  *   T is the type of the tokens the edges require
  *
  * @param neighbors
  *   is an iterator that lists all the neighbors
  */
case class Node[T](neighbors: Iterator[(T, () => Node[T])])

/** A token generator that traverses a search tree defined by a seed Node in DFS
  * fashion.
  *
  * @tparameter
  *   T is type of the tokens it generates
  */

abstract class DFS_TG[T] extends TokenGenerator[T] {
  import scala.collection.mutable.Stack
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

/** A token generator that traverses a search tree defined by a seed Node in BFS
  * fashion.
  *
  * @tparameter
  *   T is type of the tokens it generates
  */
abstract class BFS_TG[T] extends TokenGenerator[T] {
  import scala.collection.mutable.Queue
  private val levels: Queue[(List[T], Node[T])] = Queue()
  private var backtrack = false
  private var last_list: List[T] = List()
  levels.enqueue((last_list, seed))

  def seed: Node[T]

  final def suggest() = {
    if levels.isEmpty then return Finish()

    if backtrack then {
      backtrack = false
      levels.dequeue()
    }

    levels.dequeueWhile(x => !x._2.neighbors.hasNext)
    if levels.isEmpty then return Finish()

    val (ls, current) = levels.front
    val (token, nodeMaker) = current.neighbors.next()
    val action =
      if ls eq last_list then Append(token)
      else if !last_list.isEmpty && last_list.tail == ls then ReplaceLast(token)
      else Rebuild((token :: ls).reverseIterator)
    last_list = token :: ls
    levels.enqueue((last_list, nodeMaker()))
    action
  }

  final def receiveFeedback(status: ParserStatus) = status match {
    case Accepting => levels.clear()
    case Rejecting => backtrack = true
    case _         => ()
  }
}

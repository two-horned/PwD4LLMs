package pwd4llm

/** Defines the actions TokenGenerators can request. These actions are read by
  * an Evaluator and applied such that a new and correct parser state
  * correlating to the then produced token input is yielded.
  *
  * @tparameter
  *   T is type of the tokens the TokenGenerator generates
  */
enum GeneratorAction[Token] {

  /** The action to concatenate tokens with the input, the Evaluator inspects
    * currently. The new tokens are placed at the end.
    *
    * @param tokens
    *   the tokens to concantenate to the input
    */
  case Concatenate(tokens: Iterator[Token])

  /** The action to append a single token to the the input, the Evaluator
    * inspects currently.
    *
    * @param token
    *   the token to append to the input
    */
  case Append(token: Token)

  /** The action to delete a number of tokens at the back.
    *
    * @param number
    *   how many tokens need to be deleted, which should be greater 1 and
    *   smaller than the amount of all tokens the Evaluator currently inspects
    */
  case DropLast(number: Int)

  /** The action to delete the last token, which should only be a valid action,
    * if atleast one token exists in the input, the Evaluator inspects
    * currently.
    */
  case DeleteLast()

  /** The action to replace the last token with another, which should only be a
    * valid action, if atleast one token exists in the input, the Evaluator
    * inspects currently.
    *
    * @param token
    *   is the token to replace the last with
    */
  case ReplaceLast(token: Token)

  /** The action to rebuild the entire input, the Evaluator inspects currently.
    *
    * @param tokens
    *   are the tokens that become the new input
    */
  case Rebuild(tokens: Iterator[Token])

  /** The action to signal the Evaluator to finish.
    */
  case Finish()

  /** The action to reset the whole Evaluator to an empty input.
    */
  case Reset()
}

import GeneratorAction.*

/** Basic trait that denotes what a token generator is.
  *
  * @tparameter
  *   T is the type of the tokens it may generate
  */
import pwd4llm.ParserState.*

trait TokenGenerator[T] {
  def suggest(): GeneratorAction[T]
  def receiveFeedback(state: ParserState): Unit
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

  final def receiveFeedback(state: ParserState) = state match {
    case Accepting => levels.clear()
    case Failed => backtrack = true
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

  final def receiveFeedback(state: ParserState) = state match {
    case Accepting => levels.clear()
    case Failed => backtrack = true
    case _         => ()
  }
}

package pwd4llm.examples

import pwd4llm.*
import ParserState.*
import GeneratorAction.*

import fcd.PythonParsers
import PythonParsers.*
import Lexeme.*

import scala.language.implicitConversions
import scala.collection.immutable.ArraySeq
import scala.util.Random

private val token_list: ArraySeq[Lexeme] = ArraySeq(Id("xyz"), WS, NL, Punct("="), Punct("+"), Punct("*"), EOS)

/** The tools for the PythonParsers object.
  */
object PythonParsersTools extends DerivativeParserTools(PythonParsers)

class DFS_PythonTG extends DFS_TG[Lexeme] {
  private val random = new Random()

  private def node(): Node[Lexeme] =
    Node(random.shuffle(token_list).view.map(x => (x, () => node())).iterator)

  def seed() = Node(Iterator((Id("xyz"), () => node())))
}

class BFS_PythonTG extends BFS_TG[Lexeme] {
  private val random = new Random()

  private def node(): Node[Lexeme] =
    Node(random.shuffle(token_list).view.map(x => (x, () => node())).iterator)

  def seed() = Node(Iterator((Id("xyz"), () => node())))
}

class RandoPythonTokenGen extends TokenGenerator[Lexeme] {
  private val max_tokens = 30
  private val bias = 10

  private var random = new Random
  private var token_length = 0
  private var last_state = Pending

  def suggest(): GeneratorAction[Lexeme] = {
    last_state match {
      case Failed => {
        if 0 == random.nextInt(
            max_tokens - scala.math.min(max_tokens, token_length)
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
            max_tokens - scala.math.min(max_tokens - bias, token_length)
          )
        then Finish()
        else {
          token_length = 0
          Reset()
        }
      }
      case Pending => {
        if 0 == random.nextInt(
            max_tokens - scala.math.min(max_tokens, token_length)
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

  def receiveFeedback(state: ParserState): Unit = {
    last_state = state
  }
}

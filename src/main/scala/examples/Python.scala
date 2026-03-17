package pwd4llm.examples

import pwd4llm.*
import ParserState.*
import GeneratorAction.*

import fcd.PythonParsers
import PythonParsers.*
import Lexeme.*

import scala.util.Random

private val TOKEN_LIST =
  Array(Id("xyz"), WS, NL, Punct("="), Punct("+"), Punct("*"), EOS)

/** The tools for the PythonParsers object.
  */
object PythonParsersTools extends DerivativeParserTools(PythonParsers)

class DFS_PythonTG extends DFS_TG[Lexeme] {
  private val random = new Random()

  private def node(): Node[Lexeme] =
    Node(random.shuffle(TOKEN_LIST).view.map(x => (x, () => node())).iterator)

  def seed() = Node(Iterator((Id("xyz"), () => node())))
}

class BFS_PythonTG extends BFS_TG[Lexeme] {
  private val random = new Random()

  private def node(): Node[Lexeme] =
    Node(random.shuffle(TOKEN_LIST).view.map(x => (x, () => node())).iterator)

  def seed() = Node(Iterator((Id("xyz"), () => node())))
}

class RandoPythonTokenGen extends TokenGenerator[Lexeme] {
  private val MAX_TOKENS = 30
  private val BIAS = 10

  private var random = new Random
  private var token_length = 0
  private var last_state = Pending

  private var token_list =
    Array(Id("xyz"), WS, NL, Punct("="), Punct("+"), Punct("*"), EOS)

  def suggest(): GeneratorAction[Lexeme] = {
    last_state match {
      case Failed => {
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

  def receiveFeedback(state: ParserState): Unit = {
    last_state = state
  }
}

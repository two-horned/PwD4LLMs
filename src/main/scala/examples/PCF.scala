package pwd4llm.examples

import scala.language.implicitConversions
import fcd.DerivativeParsers.*

object PCF {
  enum Expr {
    case Zero
    case Id(name: String)
    case Succ(num: Expr)
    case Pred(num: Expr)
    case Abs(arg: String, argType: Type, body: Expr)
    case App(fun: Expr, arg: Expr)
    case IfThenElse(cond: Expr, th: Expr, el: Expr)
    case Fix(inner: Expr)
  }

  import Expr.*

  def isValue(expr: Expr): Boolean = expr match {
    case Zero | Abs(_, _, _) => true
    case _                   => false
  }

  enum Type {
    case Nat
    case Fun(from: Type, to: Type)
  }

  import Type.*

  /** Checks the type of an expr and returns its type, iff it exists.
    *
    * @param expr
    *   the expr to type-check.
    * @param env
    *   the environment, mapping identifiers to types.
    */
  def typecheck(expr: Expr, env: Map[String, Type] = Map()): Option[Type] =
    expr match {
      case Zero    => Some(Nat)
      case Id(x)   => env get x
      case Succ(x) => for tx <- typecheck(x, env) if tx == Nat yield Nat
      case Pred(x) => for tx <- typecheck(x, env) if tx == Nat yield Nat
      case IfThenElse(x, y, z) =>
        for {
          tx <- typecheck(x, env) if tx == Nat
          ty <- typecheck(y, env)
          tz <- typecheck(z, env)
        } yield Nat
      case Abs(x, y, z) =>
        for tz <- typecheck(z, env + (x -> y)) yield Fun(y, tz)
      case App(x, y) =>
        for {
          case Fun(tx1, tx2) <- typecheck(x, env)
          ty <- typecheck(y, env) if tx1 == ty
        } yield tx2
      case Fix(x) =>
        for case Fun(tx1, tx2) <- typecheck(x, env) if tx1 == tx2 yield tx1
    }

  /** A very simple parser for PCF-expressions, that expects a very strict
    * format.
    */
  val strict_expr: Parser[Expr] = {
    val typ: Parser[Type] = {
      val arr: Parser[String] = "->" | "→"
      val nat = ("Nat" | 'ℕ') ^^^ Nat
      lazy val fun = (level0 <~ arr) ~ level1 ^^ { case (x, y) => Fun(x, y) }
      lazy val level1: NT[Type] = fun | level0
      lazy val level0: NT[Type] = nat | '(' ~> level1 <~ ')'
      level1
    }

    val lambda: Parser[Char] = alt('\\', 'λ')
    val id_name: Parser[String] = alt("_", some(acceptIf(x => x <= 'a' && x >= 'z')))
    val id = id_name ^^ { Id(_) }
    val zero = '0' ^^^ Zero
    lazy val abs = (lambda ~> id_name <~ ':') ~ (typ <~ '.') ~ level3 ^^ {
      case ((x, y), z) => Abs(x, y, z)
    }
    lazy val iet = (level1 <~ '?') ~ (level3 <~ '~') ~ level3 ^^ {
      case ((x, y), z) => IfThenElse(x, y, z)
    }
    lazy val app = (level1 <~ ' ') ~ level0 ^^ { case (x, y) => App(x, y) }
    lazy val succ = alt('S', '↑') ~> level0 ^^ { Succ(_) }
    lazy val pred = alt('P', '↓') ~> level0 ^^ { Pred(_) }
    lazy val level3: NT[Expr] = abs | level2
    lazy val level2: NT[Expr] = iet | level1
    lazy val level1: NT[Expr] = app | level0
    lazy val level0: NT[Expr] = id | zero | succ | pred | '(' ~> level3 <~ ')'
    level3
  }

  /** A more user-friendly parser for PCF-expressions, that allows one-line
    * comments starting with `#` and allows inserting, replacing or omitting
    * arbritrary whitespace characters at any place, as long the resulting
    * expression is understood unambigously as the same.
    */
  val expr = {
    val commentTail = many(not('\n')) ~> '\n'

    def stripComments[T](p: Parser[T]): Parser[T] = {
      val stripped: NT[T] = done(p) | eat(c =>
        if c == '#' then commentTail ~> stripComments(p << ' ')
        else stripComments(p << c)
      )
      stripped
    }

    /* Used to categorize subexpressions for further processing.
     * Naming doesn't really matter, but classification heavily depends
     * on existing strict_expr parser.
     */
    enum EType {
      case Operator
      case Literal
      case OpeningBrace
      case ClosingBrace
      case Whitespace
    }

    import EType.*

    // classify character belonging to a subexpression
    def classifyChar(c: Char): EType = c match {
      case '(' | '[' | '{' =>
        OpeningBrace // some characters may not be used in our grammar
      case '}' | ']' | ')' =>
        ClosingBrace // some characters may not be used in our grammar
      case _ if c.isLetterOrDigit => Literal
      case _ if c.isWhitespace    => Whitespace
      case _                      => Operator
    }

    def stripWS[T](p: Parser[T], last_state: EType = Whitespace): Parser[T] =
      done(p) | (last_state match {
        case x @ (OpeningBrace | Whitespace) =>
          eat(c =>
            classifyChar(c) match
              case Whitespace => stripWS(p, x)
              case y          => stripWS(p << c, y)
          )
        case ClosingBrace =>
          eat(c =>
            classifyChar(c) match
              case Whitespace                    => stripWS(p, ClosingBrace)
              case x @ (OpeningBrace | Literal)  => stripWS(p << ' ' << c, x)
              case x @ (ClosingBrace | Operator) => stripWS(p << c, x)
          )
        case Literal =>
          eat(c =>
            classifyChar(c) match
              case ClosingBrace | Whitespace => stripWS(p, ClosingBrace)
              case x @ OpeningBrace          => stripWS(p << ' ' << c, x)
              case x @ (Literal | Operator)  => stripWS(p << c, x)
          )
        case Operator =>
          eat(c =>
            classifyChar(c) match
              case Whitespace => stripWS(p, Operator)
              case x          => stripWS(p << c, x)
          )
      })

    stripComments(stripWS(strict_expr))
  }

  import pwd4llm.*
  import scala.util.Random

  val TOKEN_LIST =
    Array('λ', 'ℕ', '→', '(', ')', ':', '.', '?', '~', ' ', 'x', '↑', '↓', '0')
  val START_TOKEN_LIST = Array('λ', 'ℕ', '(', 'x', '↑', '↓', '0')

  private val random = new Random()
  private def node(): Node[Char] =
    Node(random.shuffle(TOKEN_LIST).view.map(x => (x, () => node())).iterator)
  private def rand_seed() = Node(random
      .shuffle(START_TOKEN_LIST)
      .view
      .map(x => (x, () => node()))
      .iterator)

  class DFS_PCF_TG extends DFS_TG[Char] {
    def seed() = rand_seed()
  }

  class BFS_PCF_TG extends BFS_TG[Char] {
    def seed() = rand_seed()
  }

}

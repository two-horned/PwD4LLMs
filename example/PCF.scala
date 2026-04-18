package pwd4llm.example.pcf

import pwd4llm.*
import internal.*
import fcd.DerivativeParsers.*
import scala.language.implicitConversions
import scala.annotation.tailrec
import scala.collection.Factory
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArraySeq as MArraySeq
import scala.util.Random

import scala.collection.immutable.Map

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

/** A very simple parser for PCF-expressions, that expects a very strict format.
  *
  * Example parse:
  *
  * > parse(strict_expr, "⥁(λrec:ℕ→ℕ.λx:ℕ.x?↑0~mul x (rec ↓x))")
  *
  * > val res0: List[pwd4llm.examples.PCF.Expr] =
  * List(Fix(Abs(rec,Fun(Nat,Nat),Abs(x,Nat,IfThenElse(Id(x),Succ(Zero),App(App(Id(mul),Id(x)),App(Id(rec),Pred(Id(x)))))))))
  */
val strict_expr: DParser[Expr] = {
  val typ: DParser[Type] = {
    val arr: DParser[String] = "->" | "→"
    val nat = ("Nat" | 'ℕ') ^^^ Nat
    lazy val fun = (level0 <~ arr) ~ level1 ^^ { case (x, y) => Fun(x, y) }
    lazy val level1: NT[Type] = fun | level0
    lazy val level0: NT[Type] = nat | '[' ~> level1 <~ ']'
    level1
  }

  val fix = alt('&', 'Υ') | '⥁'
  val lambda = alt('\\', 'λ')
  val id_name: DParser[String] =
    alt("_", some(acceptIf(x => 'a' <= x && x <= 'z')))
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
  lazy val fixpoint = fix ~> level0 ^^ { Fix(_) }
  lazy val level3: NT[Expr] = abs | level2
  lazy val level2: NT[Expr] = iet | level1
  lazy val level1: NT[Expr] = app | level0
  lazy val level0: NT[Expr] =
    id | zero | succ | pred | fixpoint | '(' ~> level3 <~ ')'
  level3
}

/** Just like `strict_expr`, but has to end with the enter symbol (like an EOS
  * symbol)
  */
val strict_expr_enter = strict_expr <~ '↩'

/** A more user-friendly parser for PCF-expressions, that allows one-line
  * comments starting with `#` and allows inserting, replacing or omitting
  * arbritrary whitespace characters at any place, as long the resulting
  * expression is understood unambigously as the same.
  */
val expr = {
  val commentTail = many(not('\n')) ~> '\n'

  def stripComments[T](p: DParser[T]): DParser[T] = {
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

  def stripWS[T](p: DParser[T], last_state: EType = Whitespace): DParser[T] =
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

/** Just like `expr`, but has to end with the enter symbol (like an EOS symbol)
  */
val expr_enter = strict_expr <~ '↩'

def preparedMarkovChain = {
  val token_list = ArraySeq('(', ')', 'λ', 'ℕ', '→', '[', ']', ':', '.', '?',
    '~', ' ', '_', '↑', '↓', '0', '⥁')
  val tmp = MarkovChain(token_list)
  // set weights to zero for certain not-possible transitions
  val not_start_token =
    ArraySeq(')', 'ℕ', '→', ':', '.', '?', '~', ' ', '[', ']')
  for t <- not_start_token do {
    val i = tmp.indexForToken(t)
    tmp.initial(i) = 0
  }
  for i <- Iterator('(', '.', '?', '~').map(tmp.indexForToken) do {
    for t <- not_start_token do {
      val j = tmp.indexForToken(t)
      tmp.matrix(tmp.matrixIndex(i, j)) = 0
    }
  }
  {
    val allowed = ArraySeq('↑', '0', '_')
    val i = tmp.indexForToken('↑')
    for t <- token_list.iterator.filter(!allowed.contains(_)) do {
      val j = tmp.indexForToken(t)
      tmp.matrix(tmp.matrixIndex(i, j)) = 0
    }
  }
  {
    val allowed = ArraySeq('↓', '0', '_')
    val i = tmp.indexForToken('↓')
    for t <- token_list.iterator.filter(!allowed.contains(_)) do {
      val j = tmp.indexForToken(t)
      tmp.matrix(tmp.matrixIndex(i, j)) = 0
    }
  }
  {
    val i = tmp.indexForToken('λ')
    for t <- token_list.iterator.filter(_ != '_') do {
      val j = tmp.indexForToken(t)
      tmp.matrix(tmp.matrixIndex(i, j)) = 0
    }
  }
  {
    val allowed = ArraySeq(')', ':', '?', '~', ' ')
    val i = tmp.indexForToken('_')
    for t <- token_list.iterator.filter(!allowed.contains(_)) do {
      val j = tmp.indexForToken(t)
      tmp.matrix(tmp.matrixIndex(i, j)) = 0
    }
  }
  {
    val allowed = ArraySeq('[', 'ℕ')
    for i <- Iterator(':', '[', '→').map(tmp.indexForToken) do {
      for t <- token_list.iterator.filter(!allowed.contains(_)) do {
        val j = tmp.indexForToken(t)
        tmp.matrix(tmp.matrixIndex(i, j)) = 0
      }
    }
  }
  {
    val allowed = ArraySeq('→', ']', '.')
    for i <- Iterator('ℕ', ']').map(tmp.indexForToken) do {
      for t <- token_list.iterator.filter(!allowed.contains(_)) do {
        val j = tmp.indexForToken(t)
        tmp.matrix(tmp.matrixIndex(i, j)) = 0

      }
    }
  }
  {
    val allowed = ArraySeq(')', '?', '~', ' ')
    for i <- Iterator(')', '0').map(tmp.indexForToken) do {
      for t <- token_list.iterator.filter(!allowed.contains(_)) do {
        val j = tmp.indexForToken(t)
        tmp.matrix(tmp.matrixIndex(i, j)) = 0
      }
    }
  }
  {
    val allowed = ArraySeq('↑', '↓', '0', '_', '(')
    val i = tmp.indexForToken(' ')
    for t <- token_list.iterator.filter(!allowed.contains(_)) do {
      val j = tmp.indexForToken(t)
      tmp.matrix(tmp.matrixIndex(i, j)) = 0
    }
  }
  {
    val allowed = ArraySeq('_', '(')
    val i = tmp.indexForToken('⥁')
    for t <- token_list.iterator.filter(!allowed.contains(_)) do {
      val j = tmp.indexForToken(t)
      tmp.matrix(tmp.matrixIndex(i, j)) = 0
    }
  }
  tmp
}

def updateMarkovChain(
    markov_chain: MarkovChain[Char],
    validInput: String
): Unit = {
  val chars = validInput.iterator
  var last = 0
  for c <- chars.nextOption() do {
    val i = markov_chain.indexForToken(c)
    markov_chain.initial(i) += 1
    last = i
  }
  for c <- chars do {
    val i = markov_chain.indexForToken(c)
    markov_chain.matrix(markov_chain.matrixIndex(last, i)) += 1
    last = i
  }
}

/** Probabilistic Context Free Grammar for strict_expr */
final class PCFG_Node(
    context: List[Char | PCFG.Label],
    typo_rate: Double,
    rand: Random
) {

  import PCFG.*

  val (head: Option[Char], tail: List[Char | Label]) = {

    @tailrec
    def go(ctx: List[Char | Label]): (Option[Char], List[Char | Label]) =
      ctx match {
        case (x: Char) :: xs  => (Some(x), xs)
        case (x: Label) :: xs => go(nextProduct(rand, x) ++: xs)
        case _                => (None, Nil)
      }

    go(context)
  }

  private def child(ctx: List[Char | Label]): () => Node[Char] =
    () => PCFG_Node(ctx, typo_rate, rand).node()

  private def neighbors: Iterator[(Char, () => Node[Char])] = {
    Iterator
      .continually(
        head match
          case Some(h) => {
            val r = rand.nextDouble()
            // Typo mode
            if r <= typo_rate then {
              if tail != Nil then
                rand.nextInt(4) match {
                  case 0 => (h, child(h :: tail)) // Duplicate next
                  case 1 => (h, child('$' :: tail)) // Add bogus next
                  case 2 => ('%', child(tail)) // Replace with bogus
                  case 3 => (h, child(tail.tail)) // Forget next label
                }
              else
                rand.nextInt(3) match {
                  case 0 => (h, child(h :: tail)) // Duplicate next
                  case 1 => (h, child('$' :: tail)) // Add bogus next
                  case 2 => ('%', child(tail)) // Replace with bogus
                }
            } else (h, child(tail))
          }
          case None => {
            val r = rand.nextDouble()
            // Typo mode
            if r <= typo_rate then ('&', child(Nil)) // Not ending right
            else ('↩', child(Nil)) // End-of-stream
          }
      )
      .take(2) // for typo_rate=0.001 1 : 1M, there is no correct neighbor
  }

  def node(): Node[Char] = Node(neighbors)

}

def seedPCFG(
    initial_budget: Int = 10,
    typo_rate: Double = 0.01,
    rand: Random = Random()
) = {
  PCFG_Node(List(PCFG.ExprLabel(initial_budget, rand)), typo_rate, rand).node()
}

object PCFG {
  type Weight = Int
  // best pairs (8, 22/23), (4, 11), (2, 5)
  private val steps = 2
  private val shift = 5

  def nextProduct(rand: Random, label: Label): Iterable[Char | Label] = {
    val it: Iterator[Iterable[Char | Label]] =
      rand.weightedShuffle(label.productions)
    return it.next
  }

  sealed trait Label {
    def productions: Iterable[(Weight, Iterable[Char | Label])]
  }

  abstract class BudgetLabel(budget: Int, r: Random) extends Label {
    import scala.math.{max, min}

    def pies[C](pieces: Int, bc: Int)(using
        ft: Factory[Int, C]
    ): C = {
      val bg = budget - bc
      val builder = ft.newBuilder
      builder.sizeHint(pieces)
      if bg <= 0 then {
        builder.addAll(Iterator.fill(pieces)(bg))
        return builder.result
      }
      val arr = MArraySeq.fill(pieces - 1)(r.nextInt(bg))
      arr.sortInPlace()
      var n = 0
      for k <- arr do {
        builder.addOne(k - n)
        n = k
      }
      builder.addOne(bg - n)
      builder.result
    }

    def up(bias: Int): Int = {
      val b = steps * budget - shift
      (b * max(b, 0) + 1) * bias
    }

    def down(bias: Int): Int = {
      val b = steps * budget - shift
      (b * min(b, 0) + 1) * bias
    }

  }

  final class TypLabel(b: Int, r: Random) extends BudgetLabel(b, r) {
    def productions = {
      val ps: ArraySeq[Int] = pies(2, 1)
      ArraySeq(
        (up(2), ArraySeq(TypLevel0(ps(0), r), '→', TypLabel(ps(1), r))),
        (up(1) + down(4), Some(TypLevel0(b, r)))
      )
    }
  }

  final class TypLevel0(b: Int, r: Random) extends BudgetLabel(b, r) {
    def productions = ArraySeq(
      (up(1), ArraySeq('[', TypLabel(b - 2, r), ']')),
      (down(4), Some('ℕ'))
    )
  }

  final class ExprLabel(b: Int, r: Random) extends BudgetLabel(b, r) {
    def productions = {
      val ps: ArraySeq[Int] = pies(16, 4)
      ArraySeq(
        (up(12), ArraySeq('λ', IdLabel, ':', TypLabel(ps(0), r), '.', ExprLabel(ps.iterator.drop(1).sum, r))),
        (up(38) + down(49), Some(Level2(b, r)))
      )
    }
  }

  final class Level2(b: Int, r: Random) extends BudgetLabel(b, r) {
    def productions = {
      val ps: ArraySeq[Int] = pies(3, 2)
      ArraySeq(
        (up(10), ArraySeq(Level1(ps(0), r), '?', ExprLabel(ps(1), r), '~', ExprLabel(ps(2), r))),
        (up(28) + down(49), Some(Level1(b, r)))
      )
    }
  }

  final class Level1(b: Int, r: Random) extends BudgetLabel(b, r) {
    def productions = {
      val ps: ArraySeq[Int] = pies(2, 1)
      ArraySeq(
        (up(16), ArraySeq(Level1(ps(0), r), ' ', Level0(ps(1), r))),
        (up(12) + down(49), Some(Level0(b, r)))
      )
    }
  }

  final class Level0(b: Int, r: Random) extends BudgetLabel(b, r) {
    def productions = ArraySeq(
      (down(25), Some(IdLabel)),
      (down(18), Some('0')),
      (up(1) + down(3), ArraySeq('↑', Level0(b - 1, r))),
      (up(1) + down(3), ArraySeq('↓', Level0(b - 1, r))),
      (up(1), ArraySeq('⥁', Level0(b - 1, r))),
      (up(9), ArraySeq('(', ExprLabel(b - 2, r), ')'))
    )
  }

  case object IdLabel extends Label {
    def productions = ArraySeq(
      (1, Some('x')),
      (1, Some('y')),
      (1, Some('z'))
    )
  }
}

package pwd4llm.internal

import pwd4llm.*

import scala.collection.IndexedSeqOps

import scala.collection.mutable.{
  ArraySeq as MArraySeq,
  IndexedSeq as MIndexedSeq
}
import scala.collection.Factory
import scala.util.Random

extension [A](seq: MIndexedSeq[A]) {

  /** Swaps two elements in in an indexed sequents.
    *
    * @param seq
    *   indexed sequence of elements
    * @param i
    *   index of first element to swap with
    * @param j
    *   index of second element to swap with
    */

  def swap[T](i: Int, j: Int): Unit = {
    val tmp = seq(i)
    seq(i) = seq(j)
    seq(j) = tmp
  }
}

extension [A, CC[_], C](seq: IndexedSeqOps[A, CC, C]) {

  /** Performs an exponential search on an indexed sequence, so one can perform
    * ordering-preserving insertions, not guaranteed by `IndexedSeq.search`.
    *
    * @param value
    *   is the potential value to insert
    *
    * @param lower
    *   is the lower bound (included) to start searching from
    *
    * @param upper
    *   is the upper bound (excluded) to search till
    *
    * @return
    *   index in which value can be inserted
    */
  def expSearchInsPoint[B >: A](elem: B)(using ord: Ordering[B]): Int = {
    import scala.math.min

    var lower = 0
    var upper = seq.length

    while lower < upper do {
      var inc = 1

      while lower < upper && ord.lteq(seq(lower), elem) do {
        lower += inc
        inc <<= 1
      }

      inc >>= 1
      if inc == 1 then return lower
      upper = min(upper, lower)
      lower -= inc
    }

    lower
  }
}

extension (r: Random) {

  /** Performs a weighted shuffle of an iteration of items.
    *
    * @tparam T
    *   type of items
    * @tparam C
    *   type of collection the items are collected in
    * @param xs
    *   the weight-item-pairs to shuffle
    * @param ft
    *   the factory of the collection
    * @return
    *   the shuffled collection
    * @throws java.lang.IllegalArgumentException
    *   if some weight is non-positive
    */
  def weightedShuffle[T, C](
      xs: IterableOnce[(Int, T)]
  )(using ft: Factory[T, C]): C = {

    val buf: MArraySeq[(Int, T)] = MArraySeq.from(xs)
    val cummWeights: MArraySeq[Int] =
      MArraySeq.from(buf.iterator.scanLeft(0)((acc, x) => acc + x._1).drop(1))

    val builder = ft.newBuilder
    for _ <- 0 until buf.length do {
      val total = cummWeights.last
      val k = r.nextInt(total)
      val i = cummWeights.expSearchInsPoint(k)
      val (w, x) = buf(i)
      for j <- i until cummWeights.length do cummWeights(j) -= w
      builder.addOne(x)
    }
    builder.result
  }
}

/** Markov chain implementation that exposes a search tree.
  *
  * @param tokens
  *   the tokens it should handle
  */
final class MarkovChain[T](tokens: IndexedSeq[T], rand: Random = Random()) {
  val size = tokens.length
  val initial: Array[Int] = Array.fill(size)(1)
  val matrix: Array[Int] = Array.fill(size * size)(1)
  private val mapTokenToIndex: Map[T, Int] =
    tokens.iterator.zip(0 until size).toMap

  def tokenAtIndex(index: Int) = tokens(index)
  def indexForToken(token: T) = mapTokenToIndex(token)
  def matrixIndex(x: Int, y: Int): Int = x * size + y

  private def node(x: Int): Node[T] =
    Node(rand
        .weightedShuffle(
          matrix.iterator.drop(size * x).take(size).zip(0 until size).filter {
          case (x, _) => x > 0
        })
        .iterator
        .map(i => (tokens(i), () => node(i))))

  def seed(): Node[T] = Node(rand
      .weightedShuffle(
        initial.iterator.zip(0 until size).filter { case (x, _) =>
        x > 0
      })
      .iterator
      .map(i => (tokens(i), () => node(i))))
}

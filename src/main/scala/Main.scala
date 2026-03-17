import pwd4llm.*
import examples.*
import PCF.*
import fcd.DerivativeParsers.*
import DerivativeParsersTools.*

@main def main() = {
  val AT_MOST_AT_LEAST = not(atMost(5, any)) & atMost(20, any)
  val p = WrappedParser(AT_MOST_AT_LEAST &> strict_expr)

  time {
    import StackEvaluator.eval
    println("Let's go! One parse pass with DFS TG")
    val g = new DFS_PCF_TG
    println(eval(p, g))
  }

  time {
    import ScrapAllEvaluator.eval
    println("Let's go! One parse pass with DFS TG, Scrapping all")
    val g = new DFS_PCF_TG
    println(eval(p, g))
  }

  time {
    import StackEvaluator.eval
    println("Let's go! One parse pass with BFS TG")
    val g = new BFS_PCF_TG
    println(eval(p, g))
  }

  time {
    import ScrapAllEvaluator.eval
    println("Let's go! One parse pass with BFS TG, scrapping all")
    val g = new BFS_PCF_TG
    println(eval(p, g))
  }
}

def time[T](block: => T): T = {
  val before = System.nanoTime
  val result = block
  val difference = System.nanoTime - before
  if difference > 1000 then
    println("Elapsed time: " + difference / 1000000 + "ms")
  else println("Elapsed time: " + difference / 1000 + "µs")
  result
}

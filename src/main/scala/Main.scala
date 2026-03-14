import pwd4llm.*

@main def main() = {
  import PythonParsersTools.*
  import fcd.PythonParsers.*

  // {
  //   println("Let's go! One parse pass with Rando TG")
  //   val g = new RandoPythonTokenGen
  //   println(eval(p, g))
  // }

  time {
    import StackEvaluator.eval
    val p = WrappedParser(and(atMost(5, acceptIf(_ => true)),
        parsers.preprocess(parsers.file_input)))
    println("Let's go! One parse pass with DFS TG")
    val g = new DFS_PythonTG
    println(eval(p, g))
  }

  time {
    import ScrapAllEvaluator.eval
    val p = WrappedParser(and(atMost(5, acceptIf(_ => true)),
        parsers.preprocess(parsers.file_input)))
    println("Let's go! One parse pass with DFS TG, Scrapping all")
    val g = new DFS_PythonTG
    println(eval(p, g))
  }

  time {
    import StackEvaluator.eval
    val p = WrappedParser(and(not(atMost(5, acceptIf(_ => true))),
        parsers.preprocess(parsers.file_input)))
    println("Let's go! One parse pass with BFS TG")
    val g = new BFS_PythonTG
    println(eval(p, g))
  }

  time {
    import ScrapAllEvaluator.eval
    val p = WrappedParser(and(not(atMost(5, acceptIf(_ => true))),
        parsers.preprocess(parsers.file_input)))
    println("Let's go! One parse pass with BFS TG, scrapping all")
    val g = new BFS_PythonTG
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

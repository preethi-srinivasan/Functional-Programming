package recfun

/**
  * Created by preethisrinivasan on 21/07/18.
  */
object Permutations extends App{
  def permutations(s: String): List[String] = {
    def merge(ins: String, c: Char): Seq[String] =
      for (i <- 0 to ins.length) yield {
        val h = ins.substring(0, i) + c + ins.substring(i, ins.length)
        println(h)
        h
      }


    if (s.length() == 1)
      List(s)
    else {
    permutations(s.substring(0, s.length - 1)) flatMap { p =>
        println(s"Permutation start: ${p}")
        merge(p, s.charAt(s.length - 1))
      }
    }
  }
  println(permutations("abcd"))
}
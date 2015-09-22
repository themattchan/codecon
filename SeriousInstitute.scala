import scala.io._

object SI {
  def main(args: Array[String]) {
    val N = readInt()
    val S = readInt()
    val xs = for (n <- N) yield readInt()


    println(N)
    println(S)
    println(xs)
  }
}

import scala.io

object SI {



  def solve(ns: Vector[Int], s: Int) : Int = {
  //  val ns1 = { for ((a,b) <- ns zip ns.tail) yield Math.abs(a-b) }
  //val s1 = s-1
    val max = ns.last
    val min = ns.head
    val diff = max - min
    val even = diff / 2

  }

  def main(args: Array[String]) {
    val Array(n,s) = io.StdIn.readLine.split(" ").map(_.toInt)
    val ns = { for (ni <- 1 to n) yield io.StdIn.readLine.toInt }.sorted

    println(solve(ns,s))
  }
}

/* Finds erroneous entries in completed Sudokus and corrects them */
object SudokuError {
  type MIndex      = (Int, Int)          // row, col
  type IndexedCell = (Int, MIndex)       // val, idx
  type IndexedRow  = Vector[IndexedCell] // len 9
  type Grid        = Vector[IndexedRow]  // len 9

  val nums = (1 to 9).toSet

  def readSudoku() : Grid =  {
    { for (r <- 0 to 8) yield
    { val row_idxs = (0 to 8).map((r,_))
      scala.io.StdIn.readLine.split(" ").map(_.toInt).zip(row_idxs).toVector }
    }.toVector
  }

  def findDups(v: IndexedRow) : Option[List[IndexedCell]] = {
    val g = v.groupBy(_._1).filter { case (_,gd) => gd.length > 1 }
    Option(g.values.toList.flatten)
  }

  def dupsInSect(rs: Grid) : Set[IndexedCell] =
    rs.map(findDups(_)).filterNot(_.isEmpty).flatMap(_.get).toSet

  def getRows(rs: Grid) : Grid = rs
  def getCols(rs: Grid) : Grid = rs.transpose
  def get3x3s(rs: Grid) : Grid =
    rs.grouped(3)
      .map(_.transpose.grouped(3).map(_.flatten.toVector))
      .flatten.toVector

  def main(args: Array[String]) = {
    val sud = readSudoku()
    val rs  = getRows(sud)
    val cs  = getCols(sud)
    val ts  = get3x3s(sud)

    val dups1 = dupsInSect(rs) & dupsInSect(cs) & dupsInSect(ts)
    val dups  = dups1.toList.sortBy(_._2)

    for (cell <- dups) {
      cell match {
        case (n, (r,c)) => {
          val rem = rs(r).filterNot(_ == cell).map(_._1).toSet
          val List(correctNum) = (nums &~ rem).toList
          println("[" + r + "," + c + "]=" + correctNum)
        }
      }
    }
  }
}

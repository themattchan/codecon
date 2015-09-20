/* Finds erroneous entries in completed Sudokus and corrects them */
object SudokuError {
  type MIndex      = (Int, Int)
  type IndexedCell = (Int, MIndex)
  type IndexedRow  = Vector[IndexedCell]
  type Grid        = Vector[IndexedRow]

  def readSudoku() : Grid =  {
    { for (r <- 0 to 8) yield
    { val ridxs = (0 to 8).map((r,_))
      scala.io.StdIn.readLine.split(" ").map(_.toInt).zip(ridxs).toVector }
    }.toVector
  }

  def findDups(v: IndexedRow) : Option[List[IndexedCell]] = {
    val g = v.groupBy{ case (value,_) => value }.filter{ case (_,v) => v.length > 1 }
    if (g.isEmpty) None
    else           Some(g.values.toList.flatten)
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

    val dups  = dupsInSect(rs) & dupsInSect(cs) & dupsInSect(ts)
    val dups1 = dups.toList.sortBy{ case (_,idx) => idx }

    val nums = (1 to 9).toSet

    for (cell <- dups1) {
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

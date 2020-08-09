package Nonogram

import Nonogram._
import scala.io.Source

object runSolver {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) ()
    else {
      val filename = "src/test/Examples/" + args(0)
      val content = Source.fromFile(filename)
      val str = content.getLines.toList
      val (rawRows, _ :: rawCols) = str.tail.span(_ != "Collumns:")
      val rows: List[Row] = (rawRows map (r => if (r.nonEmpty) (r split " ").toList map (_.toInt) else List())).zipWithIndex.reverse
      val cols: List[Col] = rawCols map (r => if (r.nonEmpty) (r split " ").toList map (_.toInt) else List())
      println((solve(rows, cols) map show) mkString "\n")
      content.close()
    }
  }
}

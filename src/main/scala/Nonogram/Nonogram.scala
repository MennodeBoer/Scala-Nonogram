package Nonogram

object Nonogram {

  type Options = List[Int]
  type Row = (List[Int],Int)
  type Rows = List[Row]
  type Col = List[Int]
  type Cols = List[Col]
  type Fit = Set[Int]
  type Constr = Map[Int,Boolean]
  type Board = Map[Int,Map[Int,Boolean]]

  def solve(rows: Rows, cols: Cols): Set[Board] = {
    val width = cols.length
    val height = rows.length

    def partialSolve(rows: Rows): Set[(Board, Cols)] = rows match {
      case List() => Set((Map[Int,Map[Int,Boolean]](),cols))
      case (row, index) :: rs =>
        for {
        (rest, cs) <- partialSolve(rs)
        constr = addEmptyCols(rest.getOrElse(index,Map()),cs)
        fit <- fitOptions(row, width)
        if isSafe(fit, constr)
        (newValues, newCollums) = update(cs, fit, constr)
        if checkCols(newCollums, height - index)
        } yield (applyValues(rest, newValues, index), newCollums)
    }
    partialSolve(rows).map(_._1)
  }

  def fitOptions (row: Options, max: Int): Set[Fit] = {
    def fitAcc(r: Options, acc: Int) : Set[(Fit, Int)] = r match {
      case List () => Set((Set(), max))
      case v :: vs => for {
        (rest, bound) <- fitAcc(vs, acc + v + 1)
        n <- (acc to (bound - v)).toList
      }  yield (rest union (n until (n + v)).toSet, n - 1)
    }
    fitAcc(row,0).map(_._1)
  }

  def addEmptyCols(constr: Constr, cols: Cols) : Constr = {
    cols.zipWithIndex.foldRight(constr){case ((col, index), acc) =>
      if (col.isEmpty && !constr.isDefinedAt(index))
        acc ++ Map(index -> false)
      else
        acc
    }
  }

  def checkCols(cols: Cols, bound: Int) : Boolean =
    {
      def addCol(col: Col) : Int = col match{
        case List() => 0
        case c::cs =>  cs.foldRight(c)(_+_+1)
      }
      cols.map(addCol(_) <= bound).forall(identity)
    }

  def isSafe(fit: Fit, constr: Constr): Boolean = {
    val check1 = (for {
      key <- constr.keys
      if constr(key)
    } yield fit contains key).forall(identity)
    val check2 = (for {
      key <- fit
      if constr.isDefinedAt(key)
    } yield constr(key)).forall(identity)
    check1 && check2
  }

  def update(cols: Cols, fit: Fit, constr: Constr): (Map[Int,Int], Cols) = {
    cols.zipWithIndex.foldRight((Map[Int,Int](), List[Col]())){case ((col, index), (m, v)) =>
    if (constr.isDefinedAt(index) || !fit.contains(index))
      (m, col::v)
    else
      (m ++ Map(index -> col.head), col.tail::v)
    }
  }

  def applyValues(board: Board, values: Map[Int, Int], index: Int) : Board =
    {
      values.foldRight(board) { case ((col, value), b1) =>
        val end = index + value
        (index to end).foldRight(b1){case (row, b2) =>
          b2 ++ Map(row -> (b2.getOrElse(row,Map()) ++ Map(col -> (row < end))))}
      }
    }

  def show(solution: Board) : String = {
    val lines = for {
      index <- (0 to solution.keys.max).toList
      row = solution.getOrElse(index,Map(0 -> false))
      pic = row.map{ case (key,b)=>(key,if (b) "x" else " ")}
    } yield (0 to row.keys.max).map(n => pic.getOrElse(n," ")).mkString
    "\n" + (lines mkString "\n")
  }
}

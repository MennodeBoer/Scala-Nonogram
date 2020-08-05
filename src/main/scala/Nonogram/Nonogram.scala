package Nonogram

object Nonogram {

  type Row = List[Int]
  type Rows = List[Row]
  type Col = List[Int]
  type Cols = Vector[Col]
  type Fit = Map[Int,()]
  type Constr = Map[Int,Boolean]
  type Board = List[Fit]

  def solve(rows: List[Row], cols: Cols): Set[Board] = {
    val width = cols.toList.length

    def partialSolve(rows: List[Row], acc: Cols, constr: Constr): Set[Board] = rows match {
      case List() => Set(List())
      case r :: rs => for {
        fit <- fitOptions(r,0, width)
        if isSafe(fit,constr)
        cs = updateCols(acc, fit)
        newConstr = updateConstr(acc, fit)
        rest <- partialSolve(rs, cs, newConstr)
      } yield fit :: rest
    }
    partialSolve(rows, cols, Map())
  }

  def fitOptions (row: Row, min: Int, max: Int): Set[Fit] = {
    def fitAcc(r: Row, acc: Int) : Set[(Fit,Int)] = r match {
      case List () => Set((Map[Int,()]() withDefaultValue (), max))
      case v :: vs => for {
        (rest, bound) <- fitAcc(vs, acc + v + 1)
        n <- (acc to (bound - v)).toList
        m = for {p <- (n until (n + v)).toList} yield p -> ()
      }  yield (rest ++ m, n - 1)
    }
    fitAcc(row,min).map(_._1)
  }

  def isSafe(fit: Fit, constr: Constr): Boolean = {
    val check1 = (for {
      key <- constr.keys
      if constr(key)
    } yield fit.isDefinedAt(key)).forall(identity)
    val check2 = (for {
      key <- fit.keys
      if constr.isDefinedAt(key)
    } yield constr(key)).forall(identity)
    check1 && check2
  }

  def updateCols(cols: Cols, fit: Fit): Cols = {
    def updateCol(vs : List[Int]) : List[Int] = {
        val v = vs.head - 1
        if (v == 0) vs.tail else v :: vs.tail
    }
    cols.zipWithIndex.map{
      case (vs, key) => if (fit.isDefinedAt(key)) updateCol(vs) else vs
    }
  }

  def updateConstr(cols: Cols, fit: Fit): Constr = {
    (for{
      (vs, key) <- cols.zipWithIndex
      if fit.isDefinedAt(key) || vs.isEmpty
    } yield (key , vs.nonEmpty && vs.head > 1)).toMap
  }

  def show(width:Int)(solution: Board) : String = {
    val lines = for{
      fit <- solution
      pic = fit.map{ case (key,_)=>(key,"x")}
    } yield (0 to width).map(n => pic.getOrElse(n," ")).mkString
    "\n" + (lines mkString "\n")
  }
}

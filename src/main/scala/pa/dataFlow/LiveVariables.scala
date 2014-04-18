package pa.dataFlow

import pa.Analysis
import pa.ast._
import pa.controlFlow._

case class LiveVariables(prog: Stmt) extends Analysis[Set[Var]] {
  val points = programPoints(prog)
  val eBlocks = blocks(prog)
  val flows = flowGraph(prog).flows
  val programAExprs = eBlocks.map(aExprs).reduce(_ ++ _)

  override def equations(): List[Equation] = points.map {
    case pp@Exit(l) if finalLabels(prog)(l) =>
      Equation(
        pp,
        sol => Set()
      )
    case pp@Exit(l) =>
      val tos = for {(`l`, to) <- flows} yield to
      Equation(
        pp,
        sol => tos.map(l => sol(Entry(l))).reduce(_ ++ _)
      )
    case pp@Entry(l) =>
      val block = eBlocks.find(_.label == l).get
      Equation(
        pp,
        sol => sol(Exit(l)) -- kill(block) ++ gen(block)
      )
  }

  override def zeroApproximation(): Solution =
    points.map(_ -> Set[Var]()).toMap

  def kill(block: ElementaryBlock): Set[Var] = block match {
    case LabeledAssignment(x, _, _) =>
      Set(x)
    case _ => Set()
  }

  def gen(block: ElementaryBlock): Set[Var] = block match {
    case LabeledAssignment(x, ae, _) =>
      vars(ae)
    case LabeledBExpr(be, _) =>
      vars(be)
    case _ => Set()
  }
}

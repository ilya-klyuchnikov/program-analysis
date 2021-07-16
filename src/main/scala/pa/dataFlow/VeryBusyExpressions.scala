package pa.dataFlow

import pa.Analysis
import pa.ast._
import pa.controlFlow._

case class VeryBusyExpressions(prog: Stmt) extends Analysis[Set[AExpr]] {
  private val points = programPoints(prog)
  private val eBlocks = blocks(prog)
  private val flows = flowGraph(prog).flows
  private val programAExprs = eBlocks.map(aExprs).reduce(_ ++ _)

  override def equations(): List[Equation] = points.map {
    case pp @ Entry(l) =>
      val block = eBlocks.find(_.label == l).get
      Equation(
        pp,
        sol => sol(Exit(l)) -- kill(block) ++ gen(block),
      )
    case pp @ Exit(l) if finalLabels(prog).contains(l) =>
      Equation(
        pp,
        sol => Set(),
      )
    case pp @ Exit(l) =>
      val tos = for { (`l`, to) <- flows } yield to
      Equation(
        pp,
        sol => tos.map(l => sol(Entry(l))).reduce(_ & _),
      )
  }

  override def zeroApproximation(): Solution =
    points.map(_ -> programAExprs).toMap

  def kill(block: ElementaryBlock): Set[AExpr] = block match {
    case LabeledAssignment(x, _, _) =>
      programAExprs.filter(vars(_)(x))
    case _ => Set()
  }

  def gen(block: ElementaryBlock): Set[AExpr] = block match {
    case LabeledAssignment(x, ae, _) =>
      aExprs(ae)
    case LabeledBExpr(be, _) =>
      aExprs(be)
    case _ => Set()
  }
}

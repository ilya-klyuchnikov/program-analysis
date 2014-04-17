package pa.dataFlow

import pa.Analysis
import pa.ast._
import pa.controlFlow._

case class AvailableExpressions(prog: Stmt) extends Analysis[Set[AExpr]] {
  val points = programPoints(prog)
  val eBlocks = blocks(prog)
  val cfg = flowGraph(prog)
  val programAExprs = eBlocks.map(aExprs).reduce(_ ++ _)

  override def equations(): List[Equation] = points.map {
    case pp@Exit(l) =>
      val block = eBlocks.find(_.label == l).get
      Equation(
        pp,
        sol => sol(Entry(l)) -- kill(block) ++ gen(block)
      )
    case pp@Entry(l) if l == initialLabel(prog) =>
      Equation(
        pp,
        sol => Set()
      )
    case pp@Entry(l) =>
      val froms = for {(from, `l`) <- cfg.flows} yield from
      Equation(
        pp,
        sol => froms.map(from => sol(Exit(from))).reduce(_ & _)
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
      aExprs(ae).filterNot(vars(_)(x))
    case LabeledBExpr(be, _) =>
      aExprs(be)
    case _ => Set()
  }
}

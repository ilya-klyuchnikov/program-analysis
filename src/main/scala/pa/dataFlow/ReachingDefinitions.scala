package pa.dataFlow

import pa.Analysis
import pa.ast._
import pa.controlFlow._

case class ReachingDefinitions(prog: Stmt) extends Analysis[Set[(Var, Label)]] {
  val points = programPoints(prog)
  val eBlocks = blocks(prog)
  val cfg = flowGraph(prog)
  val allLabels = labels(prog) + 0
  val programVars = vars(prog)

  override def equations(): List[Equation] = points.map {
    case pp@Exit(l) =>
      eBlocks.find(_.label == l).get match {
        case LabeledAssignment(v, ae, _) =>
          Equation(
            pp,
            sol => sol(Entry(l)) -- allLabels.map(v -> _) + (v -> l)
          )
        case _ =>
          Equation(
            pp,
            sol => sol(Entry(l))
          )
      }
    case pp@Entry(l) if l == initialLabel(prog) =>
      Equation(
        pp,
        sol => programVars.map(_ -> 0).toSet
      )
    case pp@Entry(l) =>
      val froms = for {(from, `l`) <- cfg.flows} yield from
      Equation(
        pp,
        sol => froms.map(from => sol(Exit(from))).reduce(_ ++ _)
      )
  }

  override def zeroApproximation(): Solution =
    points.map(_ -> Set[(Var, Label)]()).toMap
}

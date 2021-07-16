package pa.dataFlow

import pa.Analysis
import pa.ast._
import pa.controlFlow._

case class ReachingDefinitions(prog: Stmt) extends Analysis[Set[(Var, Label)]] {
  private val points = programPoints(prog)
  private val eBlocks = blocks(prog)
  private val cfg = flowGraph(prog)
  private val allLabels = labels(prog) + 0
  private val programVars = vars(prog)

  override def equations(): List[Equation] = points.map {
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
    case pp@Exit(l) =>
      val block = eBlocks.find(_.label == l).get
      Equation(
        pp,
        sol => sol(Entry(l)) -- kill(block) ++ gen(block)
      )
  }

  override def zeroApproximation(): Solution =
    points.map(_ -> Set[(Var, Label)]()).toMap

  def kill(block: ElementaryBlock): Set[(Var, Label)] = block match {
    case LabeledAssignment(v, ae, _) =>
      allLabels.map(v -> _)
    case _ =>
      Set()
  }

  def gen(block: ElementaryBlock): Set[(Var, Label)] = block match {
    case LabeledAssignment(v, _, l) =>
      Set(v -> l)
    case _ => Set()
  }
}

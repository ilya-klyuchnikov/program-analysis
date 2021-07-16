package pa

import scala.language.implicitConversions
import pa.ast._

object dsl {
  implicit class SymbolOps(val s: Symbol) {
    def :=(e: AExpr): Assignment =
      Assignment(Var(s.name), e)
  }

  implicit def symbolToVar(s: Symbol): Var =
    Var(s.name)

  implicit def intToNumeral(i: Int): AExpr =
    Numeral(i)

  implicit class LabelOps(val label: Int) {
    def |:(assign: Assignment): Stmt =
      LabeledAssignment(assign.v, assign.ae, label)
    def |:(be: BExpr): LabeledBExpr =
      LabeledBExpr(be, label)
    def |:(s: Skip.type): Stmt =
      LabeledSkip(label)
  }

  def `while`(be: LabeledBExpr)(ss: Stmt*): Stmt =
    While(be, StmtSeq(ss.toList))

  def `if`(be: LabeledBExpr)(ss1: Stmt*)(ss2: Stmt*): Stmt =
    If(be, StmtSeq(ss1.toList), StmtSeq(ss2.toList))

  def skip: Skip.type = Skip
}

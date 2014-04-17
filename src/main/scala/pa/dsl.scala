package pa

import pa.ast._

object dsl {
  implicit class SymbolOps(val s: Symbol) {
    def :=(e: AExpr) =
      Assignment(Var(s.name), e)
  }

  implicit def symbolToVar(s: Symbol): Var =
    Var(s.name)

  implicit def intToNumeral(i: Int) =
    Numeral(i)

  implicit class LabelOps(val label: Int) {
    def |:(assign: Assignment) =
      LabeledAssignment(assign.v, assign.ae, label)
    def |:(be: BExpr) =
      LabeledBExpr(be, label)
    def |:(s: Skip.type) =
      LabeledSkip(label)
  }

  def `while`(be: LabeledBExpr)(ss: Stmt*) =
    While(be, StmtSeq(ss.toList))

  def skip = Skip
}

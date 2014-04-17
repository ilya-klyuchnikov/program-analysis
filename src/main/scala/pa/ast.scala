package pa

object ast {
  type Label = Int
  object AOperator extends Enumeration {
    type AOperator = Value
    val UNKNOWN = Value
  }

  object BOperator extends Enumeration {
    type BOperator = Value
    val UNKNOWN = Value
  }

  object ROperator extends Enumeration {
    type ROperator = Value
    val UNKNOWN = Value
  }

  import AOperator._
  import BOperator._
  import ROperator._

  // syntax
  sealed trait Expr
  sealed trait ElementaryBlock {
    val label: Label
  }

  // arithmetic expression
  sealed trait AExpr extends Expr {
    def >(ae: AExpr) =
      ROperation(this, ae, ROperator.UNKNOWN)

    def *(ae: AExpr) =
      AOperation(this, ae, AOperator.UNKNOWN)

    def -(ae: AExpr) =
      AOperation(this, ae, AOperator.UNKNOWN)
  }

  case class Var(v: String) extends AExpr
  case class Numeral(n: Int) extends AExpr
  case class AOperation(ae1: AExpr, ae2: AExpr, op: AOperator) extends AExpr

  // boolean expression
  sealed trait BExpr extends Expr
  case object True extends BExpr
  case object False extends BExpr
  case class Not(bExpr: BExpr) extends BExpr
  case class BOperation(b1: BExpr, b2: BExpr, op: BOperator) extends BExpr

  case class LabeledBExpr(be: BExpr, label: Label) extends ElementaryBlock

  // relational operation
  case class ROperation(a1: AExpr, a2: AExpr, op: ROperator) extends BExpr

  // statements = elementary blocks
  sealed trait Stmt

  case class Assignment(v: Var, ae: AExpr)
  case class LabeledAssignment(v: Var, ae: AExpr, label: Label) extends Stmt with ElementaryBlock

  case object Skip
  case class LabeledSkip(label: Label) extends Stmt with ElementaryBlock

  case class If(condition: LabeledBExpr, thenBranch: StmtSeq, elseBranch: StmtSeq) extends Stmt
  case class While(condition: LabeledBExpr, body: StmtSeq) extends Stmt
  case class StmtSeq(statements: List[Stmt]) extends Stmt

  object Program {
    def apply(statements: Stmt*): Stmt =
      if (statements.size == 1) statements(0) else StmtSeq(statements.toList)
  }

  sealed trait ProgramPoint
  case class Entry(label: Label) extends ProgramPoint
  case class Exit(label: Label) extends ProgramPoint

  def vars(stmt: Stmt): Set[Var] = stmt match {
    case LabeledAssignment(v, ae, label) =>
      Set(v) ++ vars(ae)
    case LabeledSkip(label) =>
      Set()
    case StmtSeq(statements) =>
      statements.map(vars).reduce(_ ++ _)
    case If(lbe, thenBranch, elseBranch) =>
      vars(lbe.be) ++ vars(thenBranch) ++ vars(elseBranch)
    case While(lbe, body) =>
      vars(lbe.be) ++ vars(body)
  }

  def vars(exp: Expr): Set[Var] = exp match {
    case v:Var =>
      Set(v)
    case Numeral(_) =>
      Set()
    case AOperation(ae1, ae2, _) =>
      vars(ae1) ++ vars(ae2)
    case False =>
      Set()
    case True =>
      Set()
    case Not(be) =>
      vars(be)
    case BOperation(be1, be2, _) =>
      vars(be1) ++ vars(be2)
    case ROperation(ae1, ae2, _) =>
      vars(ae1) ++ vars(ae2)
  }
}


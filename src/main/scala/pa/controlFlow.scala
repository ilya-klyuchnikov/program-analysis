package pa

import pa.ast._

object controlFlow {
  type Labels = Set[Label]
  type Flows = Set[(Label, Label)]
  case class FlowGraph(labels: Labels, flows: Flows)

  // p. 36 - initial and final labels
  def initialLabel(stmt: Stmt): Label = stmt match {
    case LabeledAssignment(v, ae, label) =>
      label
    case LabeledSkip(label) =>
      label
    case StmtSeq(statements) =>
      initialLabel(statements.head)
    case If(lbe, thenBranch, elseBranch) =>
      lbe.label
    case While(lbe, body) =>
      lbe.label
  }

  def finalLabels(stmt: Stmt): Set[Label] = stmt match {
    case LabeledAssignment(v, ae, label) =>
      Set(label)
    case LabeledSkip(label) =>
      Set(label)
    case StmtSeq(statements) =>
      finalLabels(statements.last)
    case If(lbe, thenBranch, elseBranch) =>
      finalLabels(thenBranch) ++ finalLabels(elseBranch)
    case While(lbe, body) =>
      Set(lbe.label)
  }

  // elementary blocks
  def blocks(stmt: Stmt): Set[ElementaryBlock] = stmt match {
    case assign:LabeledAssignment =>
      Set(assign)
    case skip:LabeledSkip =>
      Set(skip)
    case StmtSeq(statements) =>
      statements.map(blocks).reduce(_ ++ _)
    case If(lbe, thenBranch, elseBranch) =>
      Set(lbe) ++ blocks(thenBranch) ++ blocks(elseBranch)
    case While(lbe, body) =>
      Set(lbe) ++ blocks(body)
  }

  def labels(stmt: Stmt): Set[Label] =
    blocks(stmt).map(_.label)

  def programPoints(stmt: Stmt): List[ProgramPoint] =
    labels(stmt).toList.sorted.flatMap(l => List(Entry(l), Exit(l)))

  def flow(stmt: Stmt): Set[(Label, Label)] = stmt match {
    case LabeledAssignment(v, ae, label) =>
      Set()
    case LabeledSkip(label) =>
      Set()
    case StmtSeq(statements) =>
      val localFlow =
        statements.map(flow).reduce(_ ++ _)
      val interFlow =
        for {
          List(s1, s2) <- statements.sliding(2)
          l1 <- finalLabels(s1)
          l2 = initialLabel(s2)
        } yield l1 -> l2
      localFlow ++ interFlow
    case If(LabeledBExpr(_, lbl), b1, b2) =>
      flow(b1) ++ flow(b2) + (lbl -> initialLabel(b1)) + (lbl -> initialLabel(b2))
    case While(LabeledBExpr(_, lbl), body) =>
      flow(body) + (lbl -> initialLabel(body)) ++ finalLabels(body).map(_ -> lbl)
  }

  def reverseFlow(stmt: Stmt): Set[(Label, Label)] =
    flow(stmt).map(_.swap)

  def flowGraph(stmt: Stmt): FlowGraph =
    FlowGraph(labels(stmt), flow(stmt))

  // non-trivial arithmethic subexpressions
  def aExprs(exp: Expr): Set[AExpr] = exp match {
    case aop@AOperation(ae1, ae2, _) =>
      Set(aop) ++ aExprs(ae1) ++ aExprs(ae2)
    case Not(be) =>
      aExprs(be)
    case BOperation(be1, be2, _) =>
      aExprs(be1) ++ aExprs(be2)
    case ROperation(be1, be2, _) =>
      aExprs(be1) ++ aExprs(be2)
    case _ =>
      Set()
  }

  def aExprs(block: ElementaryBlock): Set[AExpr] = block match {
    case LabeledAssignment(v, ae, label) =>
      aExprs(ae)
    case LabeledBExpr(be, label) =>
      aExprs(be)
    case LabeledSkip(_) =>
      Set()
  }
}

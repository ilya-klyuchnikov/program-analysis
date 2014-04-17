package pa

import pa.ast._

trait Analysis[R] {
  type PP = ProgramPoint
  type Solution = Map[PP, R]
  // a single equation
  case class Equation(pp: PP, f: Solution => R)

  def equations(prog: Stmt): List[Equation]
  def zeroApproximation(prog: Stmt): Solution

  // naive solving a la chaotic iteration
  def solve(prog: Stmt): Solution = {
    var result, prev = zeroApproximation(prog)
    val eqs = equations(prog)
    do {
      prev = result
      for (eq <- eqs) {
        result = result + (eq.pp -> eq.f(result))
      }
    } while (result != prev)
    result
  }
}

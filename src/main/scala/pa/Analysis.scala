package pa

import pa.ast._

abstract class Analysis[R] {
  type PP = ProgramPoint
  type Solution = Map[PP, R]
  // a single equation
  case class Equation(pp: PP, f: Solution => R)

  def equations(): List[Equation]
  def zeroApproximation(): Solution

  // naive solving a la chaotic iteration
  def solve(): Solution = {
    var result, prev = zeroApproximation()
    val eqs = equations()
    do {
      prev = result
      for (eq <- eqs) {
        result = result + (eq.pp -> eq.f(result))
      }
    } while (result != prev)
    result
  }
}

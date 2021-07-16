package pa.test

import pa.ast._
import pa.dataFlow.LiveVariables
import pa.dsl._
import pa.controlFlow._

class LiveVariablesSpec extends org.scalatest.FunSpec {

  def set(ps: Var*): Set[Var] = Set(ps: _*)

  describe("Live Variables Analysis") {
    it("Example 2.11") {
      val prog =
        Program(
          ('x := 2) |: 1,
          ('y := 4) |: 2,
          ('x := 1) |: 3,
          `if`('y > 'x |: 4)(
            ('z := 'y) |: 5
          )(
            ('z := 'y * 'y) |: 6
          ),
          ('x := 'z) |: 7,
        )

      val actual = LiveVariables(prog).solve()
      val expected = Map[ProgramPoint, Set[Var]](
        Entry(1) -> set(),
        Exit(1) -> set(),
        Entry(2) -> set(),
        Exit(2) -> set('y),
        Entry(3) -> set('y),
        Exit(3) -> set('x, 'y),
        Entry(4) -> set('x, 'y),
        Exit(4) -> set('y),
        Entry(5) -> set('y),
        Exit(5) -> set('z),
        Entry(6) -> set('y),
        Exit(6) -> set('z),
        Entry(7) -> set('z),
        Exit(7) -> set(),
      )

      val points = programPoints(prog)
      for (p <- points) {
        assert(p -> actual(p) === p -> expected(p))
      }

      assert(actual === expected)
    }
  }

}

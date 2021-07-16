package pa.test

import pa.ast._
import pa.controlFlow._
import pa.dataFlow.AvailableExpressions
import pa.dsl._

class AvailableExpressionsSpec extends org.scalatest.FunSpec {

  describe("Available Expressions Analysis") {
    it("Example 2.5") {
      val prog =
        Program(
          ('x := 'a + 'b) |: 1,
          ('y := 'a * 'b) |: 2,
          `while`('y > 'a + 'b |: 3)(
            ('a := 'a + 1) |: 4,
            ('x := 'a + 'b) |: 5,
          ),
        )

      val actual = AvailableExpressions(prog).solve()
      val expected = Map[ProgramPoint, Set[AExpr]](
        Entry(1) -> Set(),
        Exit(1) -> Set('a + 'b),
        Entry(2) -> Set('a + 'b),
        Exit(2) -> Set('a + 'b, 'a * 'b),
        Entry(3) -> Set('a + 'b),
        Exit(3) -> Set('a + 'b),
        Entry(4) -> Set('a + 'b),
        Exit(4) -> Set(),
        Entry(5) -> Set(),
        Exit(5) -> Set('a + 'b),
      )

      val points = programPoints(prog)
      for (p <- points) {
        assert(p -> actual(p) === p -> expected(p))
      }

      assert(actual === expected)
    }
  }

}

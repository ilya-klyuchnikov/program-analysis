package pa.test

import pa.ast._
import pa.controlFlow._
import pa.dataFlow.VeryBusyExpressions
import pa.dsl._

class VeryBusyExpressionsSpec extends org.scalatest.FunSpec {

  describe("Very Busy Expressions Analysis") {
    it("Example 2.9") {
      val prog =
        Program(
          `if`('a > 'b |: 1)(
            ('x := 'b - 'a) |: 2,
            ('x := 'a - 'b) |: 3,
          )(
            ('y := 'b - 'a) |: 4,
            ('x := 'a - 'b) |: 5,
          )
        )

      val actual = VeryBusyExpressions(prog).solve()
      val expected = Map[ProgramPoint, Set[AExpr]](
        Entry(1) -> Set('a - 'b, 'b - 'a),
        Exit(1) -> Set('a - 'b, 'b - 'a),
        Entry(2) -> Set('a - 'b, 'b - 'a),
        Exit(2) -> Set('a - 'b),
        Entry(3) -> Set('a - 'b),
        Exit(3) -> Set(),
        Entry(4) -> Set('a - 'b, 'b - 'a),
        Exit(4) -> Set('a - 'b),
        Entry(5) -> Set('a - 'b),
        Exit(5) -> Set(),
      )

      val points = programPoints(prog)
      for (p <- points) {
        assert(p -> actual(p) === p -> expected(p))
      }

      assert(actual === expected)
    }
  }

}

package pa.test

import pa.ast._
import pa.dataFlow.ReachingDefinitions
import pa.dsl._
import pa.controlFlow._

class ReachingDefinitionsSpec extends org.scalatest.FunSpec {

  def set(ps: (Symbol, Label)*): Set[(Var, Label)] =
    Set(ps.map { case (s, l) => (Var(s.name), l) }: _*)

  describe("Reaching Definitions Analysis") {
    it("Example 1.1") {
      val prog =
        Program(
          ('y := 'x) |: 1,
          ('z := 1) |: 2,
          `while`('y > 1 |: 3)(
            ('z := 'z * 'y) |: 4,
            ('y := 'y - 1) |: 5,
          ),
          ('y := 0) |: 6,
        )

      val actual = ReachingDefinitions(prog).solve()
      val expected = Map[ProgramPoint, Set[(Var, Label)]](
        Entry(1) -> set('x -> 0, 'y -> 0, 'z -> 0),
        Exit(1) -> set('x -> 0, 'y -> 1, 'z -> 0),
        Entry(2) -> set('x -> 0, 'y -> 1, 'z -> 0),
        Exit(2) -> set('x -> 0, 'y -> 1, 'z -> 2),
        Entry(3) -> set('x -> 0, 'y -> 1, 'y -> 5, 'z -> 2, 'z -> 4),
        Exit(3) -> set('x -> 0, 'y -> 1, 'y -> 5, 'z -> 2, 'z -> 4),
        Entry(4) -> set('x -> 0, 'y -> 1, 'y -> 5, 'z -> 2, 'z -> 4),
        Exit(4) -> set('x -> 0, 'y -> 1, 'y -> 5, 'z -> 4),
        Entry(5) -> set('x -> 0, 'y -> 1, 'y -> 5, 'z -> 4),
        Exit(5) -> set('x -> 0, 'y -> 5, 'z -> 4),
        Entry(6) -> set('x -> 0, 'y -> 1, 'y -> 5, 'z -> 2, 'z -> 4),
        Exit(6) -> set('x -> 0, 'y -> 6, 'z -> 2, 'z -> 4),
      )

      val points = programPoints(prog)
      for (p <- points) {
        assert(p -> actual(p) === p -> expected(p))
      }

      assert(actual === expected)
    }

    it("Example 2.6") {
      val prog =
        Program(
          ('x := 5) |: 1,
          ('y := 1) |: 2,
          `while`('x > 1 |: 3)(
            ('y := 'x * 'y) |: 4,
            ('x := 'x - 1) |: 5,
          ),
        )
      val actual = ReachingDefinitions(prog).solve()
      val expected = Map[ProgramPoint, Set[(Var, Label)]](
        Entry(1) -> set('x -> 0, 'y -> 0),
        Exit(1) -> set('x -> 1, 'y -> 0),
        Entry(2) -> set('x -> 1, 'y -> 0),
        Exit(2) -> set('x -> 1, 'y -> 2),
        Entry(3) -> set('x -> 1, 'y -> 2, 'y -> 4, 'x -> 5),
        Exit(3) -> set('x -> 1, 'y -> 2, 'y -> 4, 'x -> 5),
        Entry(4) -> set('x -> 1, 'y -> 2, 'y -> 4, 'x -> 5),
        Exit(4) -> set('x -> 1, 'y -> 4, 'x -> 5),
        Entry(5) -> set('x -> 1, 'y -> 4, 'x -> 5),
        Exit(5) -> set('y -> 4, 'x -> 5),
      )

      val points = programPoints(prog)
      for (p <- points) {
        assert(p -> actual(p) === p -> expected(p))
      }

      assert(actual === expected)
    }
  }
}

import Types._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestSolve extends FunSuite {
  val db = Map(
    (Word("test"), 0) -> List(
      new Rule(Atom(Word("test")))
    ),
    (Word("left-of"), 2) -> List(
      new Rule(Atom(Word("left-of"), List(Word("jill"), Word("bob")))),
      new Rule(Atom(Word("left-of"), List(Word("bob"), Word("tony")))),
      new Rule(Atom(Word("left-of"), List(Word("tony"), Word("tracey")))),
      new Rule(Atom(Word("left-of"), List(Word("tracey"), Word("ian")))),
      new Rule(Atom(Word("left-of"), List(Word("ian"), Word("mary")))),
      new Rule(Atom(Word("left-of"), List(Word("mary"), Word("sam"))))
    ),
    (Word("mutters"), 1) -> List(
      new Rule(Atom(Word("mutters"), List(Word("jill")))),
      new Rule(Atom(Word("mutters"), List(Word("mary"))))
    ),
    (Word("cheers"), 1) -> List(
      new Rule(Atom(Word("cheers"), List(Word("bob")))),
      new Rule(Atom(Word("cheers"), List(Word("tracey")))),
      new Rule(Atom(Word("cheers"), List(Word("ian"))))
    ),
    (Word("dozes"), 1) -> List(
      new Rule(Atom(Word("dozes"), List(Word("tony")))),
      new Rule(Atom(Word("dozes"), List(Word("sam"))))
    ),
    (Word("beside"), 2) -> List(
      new Rule(Atom(Word("beside"), List(Variable("X"), Variable("Y"))),
        Atom(Word("left-of"), List(Variable("X"), Variable("Y")))),
      new Rule(Atom(Word("beside"), List(Variable("X"), Variable("Y"))),
        Atom(Word("left-of"), List(Variable("Y"), Variable("X"))))
    ),
    (Word("happy"), 1) -> List(
      new Rule(Atom(Word("happy"), List(Variable("X"))),
        Atom(Word("cheers"), List(Variable("X"))))
    ),
    (Word("disturbed-by"), 2) -> List(
      new Rule(Atom(Word("disturbed-by"), List(Variable("X"), Variable("Y"))),
        Conj(List(
          Atom(Word("dozes"), List(Variable("X"))),
          Atom(Word("beside"), List(Variable("X"), Variable("Y"))),
          Atom(Word("cheers"), List(Variable("Y"))))
        ))
    )
  )

  val q = new Query(db)

  test("test") {
    assert(q.solve(Atom(Word("test"))).contains(List(Map())))
  }

  test("bob left-of tony") {
    assert(q.solve(Atom(Word("left-of"), List(Word("bob"), Word("tony")))).contains(List(Map())))
  }

  test("tony left-of bob") {
    assert(q.solve(Atom(Word("left-of"), List(Word("tony"), Word("bob")))).isEmpty)
  }

  test("X happy") {
    assert(q.solve(Atom(Word("happy"), List(Variable("X")))).contains(List(
      Map(Variable("X") -> Word("bob")),
      Map(Variable("X") -> Word("tracey")),
      Map(Variable("X") -> Word("ian"))
    )))
  }

  test("X beside bob") {
    assert(q.solve(Atom(Word("beside"), List(Variable("X"), Variable("bob")))).contains(List(
      Map(Variable("X") -> Word("jill")),
      Map(Variable("X") -> Word("tony"))
    )))
  }

  test("NOT jill mutters") {
    assert(q.solve(Not(Atom(Word("mutters"), List(Word("jill"))))).isEmpty)
  }

  test("X mutters AND X left-of Y") {
    assert(q.solve(Conj(List(
      Atom(Word("mutters"), List(Variable("X"))),
      Atom(Word("left-of"), List(Variable("X"), Variable("Y")))
    ))).contains(List(
      Map(Variable("X") -> Word("jill"), Variable("Y") -> Word("bob")),
      Map(Variable("X") -> Word("mary"), Variable("Y") -> Word("sam"))
    )))
  }

  test("tony disturbed-by X") {
    assert(q.solve(Atom(Word("disturbed-by"), List(Word("tony"), Variable("X")))).contains(List(
      Map(Variable("X") -> Word("tracey")),
      Map(Variable("X") -> Word("bob"))
    )))
  }
}

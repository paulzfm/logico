import Types._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestSolve extends FunSuite {
  val db = new Database(List(
    Rule(Atom(Word("test"))),
    Rule(Atom(Word("left-of"), List(Word("jill"), Word("bob")))),
    Rule(Atom(Word("left-of"), List(Word("bob"), Word("tony")))),
    Rule(Atom(Word("left-of"), List(Word("tony"), Word("tracey")))),
    Rule(Atom(Word("left-of"), List(Word("tracey"), Word("ian")))),
    Rule(Atom(Word("left-of"), List(Word("ian"), Word("mary")))),
    Rule(Atom(Word("left-of"), List(Word("mary"), Word("sam")))),
    Rule(Atom(Word("mutters"), List(Word("jill")))),
    Rule(Atom(Word("mutters"), List(Word("mary")))),
    Rule(Atom(Word("cheers"), List(Word("bob")))),
    Rule(Atom(Word("cheers"), List(Word("tracey")))),
    Rule(Atom(Word("cheers"), List(Word("ian")))),
    Rule(Atom(Word("dozes"), List(Word("tony")))),
    Rule(Atom(Word("dozes"), List(Word("sam")))),
    Rule(Atom(Word("beside"), List(Variable("X"), Variable("Y"))),
      Atom(Word("left-of"), List(Variable("X"), Variable("Y")))),
    Rule(Atom(Word("beside"), List(Variable("X"), Variable("Y"))),
      Atom(Word("left-of"), List(Variable("Y"), Variable("X")))),
    Rule(Atom(Word("happy"), List(Variable("X"))),
      Atom(Word("cheers"), List(Variable("X")))),
    Rule(Atom(Word("disturbed-by"), List(Variable("X"), Variable("Y"))),
      Conj(List(
        Atom(Word("dozes"), List(Variable("X"))),
        Atom(Word("beside"), List(Variable("X"), Variable("Y"))),
        Atom(Word("cheers"), List(Variable("Y"))))
      )
    )
  ))

  val q = new Query(db)

  println(db)

  test("test") {
    val (results, trace) = q.solve(Atom(Word("test")))
    println(trace)
    assert(results == List(Map()))
  }

  test("bob left-of tony") {
    val (results, trace) = q.solve(Atom(Word("left-of"), List(Word("bob"), Word("tony"))))
    println(trace)
    assert(results == List(Map()))
  }

  test("tony left-of bob") {
    val (results, trace) = q.solve(Atom(Word("left-of"), List(Word("tony"), Word("bob"))))
    println(trace)
    assert(results.isEmpty)
  }

  test("X happy") {
    val (results, trace) = q.solve(Atom(Word("happy"), List(Variable("X"))))
    println(trace)
    assert(results == List(
      Map(Variable("X") -> Word("bob")),
      Map(Variable("X") -> Word("tracey")),
      Map(Variable("X") -> Word("ian"))
    ))

  }

  test("X beside bob") {
    val (results, trace) = q.solve(Atom(Word("beside"), List(Variable("X"), Variable("bob"))))
    println(trace)
    assert(results == List(
      Map(Variable("X") -> Word("jill")),
      Map(Variable("X") -> Word("tony"))
    ))
  }

  test("NOT jill mutters") {
    val (results, trace) = q.solve(Not(Atom(Word("mutters"), List(Word("jill")))))
    println(trace)
    assert(results.isEmpty)
  }

  test("X mutters AND X left-of Y") {
    val (results, trace) = q.solve(Conj(List(
      Atom(Word("mutters"), List(Variable("X"))),
      Atom(Word("left-of"), List(Variable("X"), Variable("Y")))
    )))
    println(trace)
    assert(results == List(
      Map(Variable("X") -> Word("jill"), Variable("Y") -> Word("bob")),
      Map(Variable("X") -> Word("mary"), Variable("Y") -> Word("sam"))
    ))
  }

  test("tony disturbed-by X") {
    val (results, trace) = q.solve(Atom(Word("disturbed-by"), List(Word("tony"), Variable("X"))))
    println(trace)
    assert(results == List(
      Map(Variable("X") -> Word("tracey")),
      Map(Variable("X") -> Word("bob"))
    ))
  }
}

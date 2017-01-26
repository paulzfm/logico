import RandomTokens.resetTmpToken
import Types._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite}

@RunWith(classOf[JUnitRunner])
class TestSolve extends FunSuite with BeforeAndAfter {
  val db = new Database(List(
    new Rule(Atom(Word("test"))),
    new Rule(Atom(Word("left-of"), List(Word("jill"), Word("bob")))),
    new Rule(Atom(Word("left-of"), List(Word("bob"), Word("tony")))),
    new Rule(Atom(Word("left-of"), List(Word("tony"), Word("tracey")))),
    new Rule(Atom(Word("left-of"), List(Word("tracey"), Word("ian")))),
    new Rule(Atom(Word("left-of"), List(Word("ian"), Word("mary")))),
    new Rule(Atom(Word("left-of"), List(Word("mary"), Word("sam")))),
    new Rule(Atom(Word("mutters"), List(Word("jill")))),
    new Rule(Atom(Word("mutters"), List(Word("mary")))),
    new Rule(Atom(Word("cheers"), List(Word("bob")))),
    new Rule(Atom(Word("cheers"), List(Word("tracey")))),
    new Rule(Atom(Word("cheers"), List(Word("ian")))),
    new Rule(Atom(Word("dozes"), List(Word("tony")))),
    new Rule(Atom(Word("dozes"), List(Word("sam")))),
    new Rule(Atom(Word("beside"), List(Variable("X"), Variable("Y"))),
      Atom(Word("left-of"), List(Variable("X"), Variable("Y")))),
    new Rule(Atom(Word("beside"), List(Variable("X"), Variable("Y"))),
      Atom(Word("left-of"), List(Variable("Y"), Variable("X")))),
    new Rule(Atom(Word("happy"), List(Variable("X"))),
      Atom(Word("cheers"), List(Variable("X")))),
    new Rule(Atom(Word("disturbed-by"), List(Variable("X"), Variable("Y"))),
      Conj(List(
        Atom(Word("dozes"), List(Variable("X"))),
        Atom(Word("beside"), List(Variable("X"), Variable("Y"))),
        Atom(Word("cheers"), List(Variable("Y"))))
      )
    ),
    new Rule(Atom(Word("is-a-thing"), List(Variable("X"))))
  ))

  val q = new Query(db)

  println(db)

  before {
    resetTmpToken()
  }

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
    val (results, trace) = q.solve(Atom(Word("beside"), List(Variable("X"), Word("bob"))))
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

  test("X mutters AND X left-of _") {
    val (results, trace) = q.solve(Conj(List(
      Atom(Word("mutters"), List(Variable("X"))),
      Atom(Word("left-of"), List(Variable("X"), Any))
    )))
    println(trace)
    assert(results == List(
      Map(Variable("X") -> Word("jill")),
      Map(Variable("X") -> Word("mary"))
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

  test("cat is-a-thing") {
    val (results, trace) = q.solve(Atom(Word("is-a-thing"), List(Word("cat"))))
    println(trace)
    assert(results == List(Map()))
  }

  test("123 is-a-thing") {
    val (results, trace) = q.solve(Atom(Word("is-a-thing"), List(Integer(123))))
    println(trace)
    assert(results == List(Map()))
  }

  test("X is-a-thing") {
    val (results, trace) = q.solve(Atom(Word("is-a-thing"), List(Variable("X"))))
    println(trace)
    assert(results == List(Map()))
  }
}

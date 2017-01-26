import Types._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestBuiltIns extends FunSuite {
  val q = new Query

  test("1 + 11 = 12") {
    val (results, trace) = q.solve(Atom(Word("sum"),
      List(Integer(1), Integer(11), Integer(12))))
    println(trace)
    assert(results == List(Map()))
  }

  test("1 + 11 = X") {
    val (results, trace) = q.solve(Atom(Word("sum"),
      List(Integer(1), Integer(11), Variable("X"))))
    println(trace)
    assert(results == List(Map(Variable("X") -> Integer(12))))
  }

  test("X + 11 = 12") {
    val (results, trace) = q.solve(Atom(Word("sum"),
      List(Variable("X"), Integer(11), Integer(12))))
    println(trace)
    assert(results == List(Map(Variable("X") -> Integer(1))))
  }

  test("1 + X = 12") {
    val (results, trace) = q.solve(Atom(Word("sum"),
      List(Integer(1), Variable("X"), Integer(12))))
    println(trace)
    assert(results == List(Map(Variable("X") -> Integer(11))))
  }

  test("2 * 3 = 6") {
    val (results, trace) = q.solve(Atom(Word("times"),
      List(Integer(2), Integer(3), Integer(6))))
    println(trace)
    assert(results == List(Map()))
  }

  test("2 * 3 = X") {
    val (results, trace) = q.solve(Atom(Word("times"),
      List(Integer(2), Integer(3), Variable("X"))))
    println(trace)
    assert(results == List(Map(Variable("X") -> Integer(6))))
  }

  test("X * 3 = 6") {
    val (results, trace) = q.solve(Atom(Word("times"),
      List(Variable("X"), Integer(3), Integer(6))))
    println(trace)
    assert(results == List(Map(Variable("X") -> Integer(2))))
  }

  test("2 * X = 6") {
    val (results, trace) = q.solve(Atom(Word("times"),
      List(Integer(2), Variable("X"), Integer(6))))
    println(trace)
    assert(results == List(Map(Variable("X") -> Integer(3))))
  }

  test("3 < 2 ?") {
    val (results, trace) = q.solve(Atom(Word("lt"), List(Integer(3), Integer(2))))
    println(trace)
    assert(results.isEmpty)
  }

  test("3 < 3 ?") {
    val (results, trace) = q.solve(Atom(Word("lt"), List(Integer(3), Integer(3))))
    println(trace)
    assert(results.isEmpty)
  }

  test("3 < 4") {
    val (results, trace) = q.solve(Atom(Word("lt"), List(Integer(3), Integer(4))))
    println(trace)
    assert(results == List(Map()))
  }

  test("2 = 2") {
    val (results, trace) = q.solve(Atom(Word("eq"), List(Integer(2), Integer(2))))
    println(trace)
    assert(results == List(Map()))
  }

  test("2 /= 3") {
    val (results, trace) = q.solve(Atom(Word("eq"), List(Integer(2), Integer(3))))
    println(trace)
    assert(results.isEmpty)
  }

  test("cat = cat") {
    val (results, trace) = q.solve(Atom(Word("eq"), List(Word("cat"), Word("cat"))))
    println(trace)
    assert(results == List(Map()))
  }

  test("[1,2,3] = [1,2,3]") {
    val (results, trace) = q.solve(Atom(Word("eq"), List(
      CList(List(Integer(1), Integer(2), Integer(3))),
      CList(List(Integer(1), Integer(2), Integer(3)))
    )))
    println(trace)
    assert(results == List(Map()))
  }
}
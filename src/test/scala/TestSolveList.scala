import Types._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestSolveList extends FunSuite {
  val stdListDb = new Database(List(
    new Rule(Atom(Word("member-of"), List(Variable("X"),
      PList(List(Variable("X")), Any)))),
    new Rule(Atom(Word("member-of"), List(Variable("X"),
      PList(List(Any), Variable("Ys")))),
      Atom(Word("member-of"), List(Variable("X"), Variable("Ys")))),

    new Rule(Atom(Word("length-is"), List(CList(), Integer(0)))),
    new Rule(Atom(Word("length-is"), List(
      PList(List(Variable("X")), Variable("XS")), Variable("Y")
    )), Conj(List(
      Atom(Word("length-is"), List(Variable("XS"), Variable("Y1"))),
      Atom(Word("sum"), List(Variable("Y1"), Integer(1), Variable("Y")))
    )))
  ))

  val q = new Query(stdListDb)

  println(stdListDb)

  test("3 member-of [1,2,3,4]") {
    val (results, trace) = q.solve(Atom(Word("member-of"), List(Integer(3), CList(List(
      Integer(1),
      Integer(2),
      Integer(3),
      Integer(4)
    )))))
    println(trace)
    assert(results == List(Map()))
  }

  test("3 NOT member-of [1,2,4,5]") {
    val (results, trace) = q.solve(Atom(Word("member-of"), List(Integer(3), CList(List(
      Integer(1),
      Integer(2),
      Integer(4),
      Integer(5)
    )))))
    println(trace)
    assert(results.isEmpty)
  }

  test("X member-of [1,2,3,4]") {
    val (results, trace) = q.solve(Atom(Word("member-of"), List(Variable("X"), CList(List(
      Integer(1),
      Integer(2),
      Integer(3),
      Integer(4)
    )))))
    println(trace)
    assert(results == List(
      Map(Variable("X") -> Integer(1)),
      Map(Variable("X") -> Integer(2)),
      Map(Variable("X") -> Integer(3)),
      Map(Variable("X") -> Integer(4))
    ))
  }

  test("X member-of []") {
    val (results, trace) = q.solve(Atom(Word("member-of"), List(Variable("X"), CList(List()))))
    println(trace)
    assert(results.isEmpty)
  }

  test("[1,2,3] length-of 3") {
    val (results, trace) = q.solve(Atom(Word("length-is"), List(
      CList(List(Integer(1), Integer(2), Integer(3))),
      Integer(3)
    )))
    println(trace)
    assert(results == List(Map()))
  }

  test("[1,2,3] NOT length-of 4") {
    val (results, trace) = q.solve(Atom(Word("length-is"), List(
      CList(List(Integer(1), Integer(2), Integer(3))),
      Integer(4)
    )))
    println(trace)
    assert(results.isEmpty)
  }

  test("[1,2,3] NOT length-of 2") {
    val (results, trace) = q.solve(Atom(Word("length-is"), List(
      CList(List(Integer(1), Integer(2), Integer(3))),
      Integer(2)
    )))
    println(trace)
    assert(results.isEmpty)
  }

  test("[1,2,3] length-of X") {
    val (results, trace) = q.solve(Atom(Word("length-is"), List(
      CList(List(Integer(1), Integer(2), Integer(3))),
      Variable("X")
    )))
    println(trace)
    assert(results == List(Map(Variable("X") -> Integer(3))))
  }
}
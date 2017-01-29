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
    ))),

    new Rule(Atom(Word("append-to"), List(CList(), Variable("Y"), Variable("Y")))),
    new Rule(Atom(Word("append-to"), List(
      PList(List(Variable("X1")), Variable("XS1")),
      Variable("Y"),
      PList(List(Variable("X1")), Variable("Z"))
    )), Atom(Word("append-to"), List(Variable("XS1"), Variable("Y"), Variable("Z")))),

    new Rule(Atom(Word("position-is"), List(Variable("X"), Variable("XS"), Variable("Y"))),
      Conj(List(
        Atom(Word("append-to"),
          List(Variable("X1"), PList(List(Variable("X")), Any), Variable("XS"))),
        Atom(Word("length-is"), List(Variable("X1"), Variable("Y1"))),
        Atom(Word("sum"), List(Variable("Y1"), Integer(1), Variable("Y")))
      )))
  ))

  val q = new Solver(stdListDb)

  println(stdListDb)

  def stringList(str: String): CList = CList(str.map(_.toString).toList.map(Word))

  def intList(ints: List[Int]): CList = CList(ints.map(Integer))

  def intListPattern(ints: List[Int], v: String): PList = PList(ints.map(Integer), Variable(v))

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

  test("[1] ++ [2,3] = [1,2,3]") {
    val (results, trace) = q.solve(Atom(Word("append-to"), List(
      CList(List(Integer(1))),
      CList(List(Integer(2), Integer(3))),
      CList(List(Integer(1), Integer(2), Integer(3)))
    )))
    println(trace)
    assert(results == List(Map()))
  }

  test("[1,2,3] ++ [] = [1,2,3]") {
    val (results, trace) = q.solve(Atom(Word("append-to"), List(
      CList(List(Integer(1), Integer(2), Integer(3))),
      CList(),
      CList(List(Integer(1), Integer(2), Integer(3)))
    )))
    println(trace)
    assert(results == List(Map()))
  }

  test("[] ++ [1,2,3] = [1,2,3]") {
    val (results, trace) = q.solve(Atom(Word("append-to"), List(
      CList(),
      CList(List(Integer(1), Integer(2), Integer(3))),
      CList(List(Integer(1), Integer(2), Integer(3)))
    )))
    println(trace)
    assert(results == List(Map()))
  }

  test("[] ++ [] = []") {
    val (results, trace) = q.solve(Atom(Word("append-to"), List(CList(), CList(), CList())))
    println(trace)
    assert(results == List(Map()))
  }

  test("[1,2] ++ [3,4] = X") {
    val (results, trace) = q.solve(Atom(Word("append-to"), List(
      CList(List(Integer(1), Integer(2))),
      CList(List(Integer(3), Integer(4))),
      Variable("X")
    )))
    println(trace)
    assert(results == List(Map(Variable("X") -> CList(List(
      Integer(1), Integer(2), Integer(3), Integer(4)
    )))))
  }

  test("[1,2] ++ X = [1,2,3,4]") {
    val (results, trace) = q.solve(Atom(Word("append-to"), List(
      CList(List(Integer(1), Integer(2))),
      Variable("X"),
      CList(List(Integer(1), Integer(2), Integer(3), Integer(4)))
    )))
    println(trace)
    assert(results == List(Map(Variable("X") -> CList(List(Integer(3), Integer(4))))))
  }

  test("X ++ [3,4] = [1,2,3,4]") {
    val (results, trace) = q.solve(Atom(Word("append-to"), List(
      Variable("X"),
      CList(List(Integer(3), Integer(4))),
      CList(List(Integer(1), Integer(2), Integer(3), Integer(4)))
    )))
    println(trace)
    assert(results == List(Map(Variable("X") -> CList(List(Integer(1), Integer(2))))))
  }

  test("X1 ++ X2 = [c,o,w]") {
    val (results, trace) = q.solve(Atom(Word("append-to"), List(
      Variable("X1"),
      Variable("X2"),
      CList(List(Word("c"), Word("o"), Word("w")))
    )))
    println(trace)
    assert(results == List(
      Map(Variable("X1") -> CList(),
        Variable("X2") -> CList(List(Word("c"), Word("o"), Word("w")))),
      Map(Variable("X1") -> CList(List(Word("c"))),
        Variable("X2") -> CList(List(Word("o"), Word("w")))),
      Map(Variable("X1") -> CList(List(Word("c"), Word("o"))),
        Variable("X2") -> CList(List(Word("w")))),
      Map(Variable("X1") -> CList(List(Word("c"), Word("o"), Word("w"))),
        Variable("X2") -> CList())
    ))
  }

  test("X1 ++ i:X2 = [s,i,l,i,c,o,n]") {
    val (results, trace) = q.solve(Atom(Word("append-to"), List(
      Variable("X1"),
      PList(List(Word("i")), Variable("X2")),
      stringList("silicon")
    )))
    println(trace)
    assert(results == List(
      Map(Variable("X1") -> stringList("s"), Variable("X2") -> stringList("licon")),
      Map(Variable("X1") -> stringList("sil"), Variable("X2") -> stringList("con"))
    ))
  }

  test("6 [2,4,6,8] position-is 3") {
    val (results, trace) = q.solve(Atom(Word("position-is"), List(
      Integer(6),
      intList(List(2, 4, 6, 8)),
      Integer(3)
    )))
    println(trace)
    assert(results == List(Map()))
  }

  test("5 [1,2,3,4,5] position-is X") {
    val (results, trace) = q.solve(Atom(Word("position-is"), List(
      Integer(5),
      intList(1 to 5 toList),
      Variable("X")
    )))
    println(trace)
    assert(results == List(Map(Variable("X") -> Integer(5))))
  }

  test("6 [1,2,3,4,5] position-is X") {
    val (results, trace) = q.solve(Atom(Word("position-is"), List(
      Integer(6),
      intList(1 to 5 toList),
      Variable("X")
    )))
    println(trace)
    assert(results.isEmpty)
  }

  test("X [9,8,7,6,5] position-is 3") {
    val (results, trace) = q.solve(Atom(Word("position-is"), List(
      Variable("X"),
      intList((5 to 9 toList).reverse),
      Integer(3)
    )))
    println(trace)
    assert(results == List(Map(Variable("X") -> Integer(7))))
  }

  test("X [9,8,7,6,5] position-is 6") {
    val (results, trace) = q.solve(Atom(Word("position-is"), List(
      Variable("X"),
      intList((5 to 9 toList).reverse),
      Integer(6)
    )))
    println(trace)
    assert(results.isEmpty)
  }

  test("X [9,8,7,6,5] position-is 0") {
    val (results, trace) = q.solve(Atom(Word("position-is"), List(
      Variable("X"),
      intList((5 to 9 toList).reverse),
      Integer(0)
    )))
    println(trace)
    assert(results.isEmpty)
  }
}
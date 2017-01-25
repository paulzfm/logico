import Types._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestMatch extends FunSuite {
  val q = new Query

  test("_ = 1") {
    assert(q.matchTerms(Any, Integer(1)).contains(Map()))
  }

  test("1 = _") {
    assert(q.matchTerms(Integer(1), Any).contains(Map()))
  }

  test("_ = cat") {
    assert(q.matchTerms(Any, Word("cat")).contains(Map()))
  }

  test("cat = _") {
    assert(q.matchTerms(Word("cat"), Any).contains(Map()))
  }

  test("_ = x") {
    assert(q.matchTerms(Any, Variable("x")).contains(Map()))
  }

  test("X = _") {
    assert(q.matchTerms(Variable("X"), Any).contains(Map()))
  }

  test("_ = [[]]") {
    assert(q.matchTerms(Any, CList(List(CList()))).contains(Map()))
  }

  test("[[]] = _") {
    assert(q.matchTerms(CList(List(CList())), Any).contains(Map()))
  }

  test("1 = x") {
    assert(q.matchTerms(Integer(1), Variable("x")).contains(Map(Variable("x") -> Integer(1))))
  }

  test("X = 1") {
    assert(q.matchTerms(Variable("X"), Integer(1)).contains(Map(Variable("X") -> Integer(1))))
  }

  test("cat = x") {
    assert(q.matchTerms(Word("cat"), Variable("x")).contains(Map(Variable("x") -> Word("cat"))))
  }

  test("X = cat") {
    assert(q.matchTerms(Variable("X"), Word("cat")).contains(Map(Variable("X") -> Word("cat"))))
  }

  test("X = x") {
    assert(q.matchTerms(Variable("X"), Variable("x")).contains(Map(Variable("x") -> Variable("X"))))
  }

  test("X = X") {
    assert(q.matchTerms(Variable("X"), Variable("X")).contains(Map()))
  }

  test("[[]] = x") {
    assert(q.matchTerms(CList(List(CList())), Variable("x")).contains(
      Map(Variable("x") -> CList(List(CList())))
    ))
  }

  test("X = [[]]") {
    assert(q.matchTerms(Variable("X"), CList(List(CList()))).contains(
      Map(Variable("X") -> CList(List(CList())))
    ))
  }

  test("X:Y = x") {
    assert(q.matchTerms(PList(List(Variable("X")), Variable("Y")), Variable("x")).contains(
      Map(Variable("x") -> PList(List(Variable("X")), Variable("Y")))
    ))
  }

  test("X = x:y") {
    assert(q.matchTerms(Variable("X"), PList(List(Variable("x")), Variable("y"))).contains(
      Map(Variable("X") -> PList(List(Variable("x")), Variable("y")))
    ))
  }

  test("cat = cat") {
    assert(q.matchTerms(Word("cat"), Word("cat")).contains(Map()))
  }

  test("cat /= dog") {
    assert(q.matchTerms(Word("cat"), Word("dog")).isEmpty)
  }

  test("1 = 1") {
    assert(q.matchTerms(Integer(1), Integer(1)).contains(Map()))
  }

  test("1 /= 11") {
    assert(q.matchTerms(Integer(1), Integer(11)).isEmpty)
  }

  test("[1,2,3] = [1,2,3]") {
    assert(q.matchTerms(CList(List(Integer(1), Integer(2), Integer(3))),
      CList(List(Integer(1), Integer(2), Integer(3)))).contains(Map()))
  }

  test("[1,2,3] /= [1,2]") {
    assert(q.matchTerms(CList(List(Integer(1), Integer(2), Integer(3))),
      CList(List(Integer(1), Integer(2)))).isEmpty)
  }

  test("[1,X,Y] = [x,2,3]") {
    assert(q.matchTerms(
      CList(List(Integer(1), Variable("X"), Variable("Y"))),
      CList(List(Variable("x"), Integer(2), Integer(3)))
    ).contains(Map(
      Variable("X") -> Integer(2),
      Variable("Y") -> Integer(3),
      Variable("x") -> Integer(1)
    )))
  }

  test("X:XS = [1,2,3]") {
    assert(q.matchTerms(
      PList(List(Variable("X")), Variable("XS")),
      CList(List(Integer(1), Integer(2), Integer(3)))
    ).contains(Map(
      Variable("X") -> Integer(1),
      Variable("XS") -> CList(List(Integer(2), Integer(3)))
    )))
  }

  test("X:XS = [[]]") {
    assert(q.matchTerms(
      PList(List(Variable("X")), Variable("XS")),
      CList(List(CList()))
    ).contains(Map(Variable("X") -> CList(), Variable("XS") -> CList())))
  }

  test("[X,X,Y] = [x,1,x]") {
    assert(q.matchTerms(
      CList(List(Variable("X"), Variable("X"), Variable("Y"))),
      CList(List(Variable("x"), Integer(1), Variable("x")))
    ).contains(Map(
      Variable("X") -> Integer(1),
      Variable("Y") -> Integer(1),
      Variable("x") -> Integer(1)
    )))
  }

  test("[X,X,Y] = [x,y,x]") {
    assert(q.matchTerms(
      CList(List(Variable("X"), Variable("X"), Variable("Y"))),
      CList(List(Variable("x"), Variable("y"), Variable("x")))
    ).contains(Map(
      Variable("x") -> Variable("X"),
      Variable("y") -> Variable("X"),
      Variable("X") -> Variable("Y")
    )))
  }
}
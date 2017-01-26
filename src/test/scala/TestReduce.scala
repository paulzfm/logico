import RandomTokens.resetTmpToken
import Types._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite}

@RunWith(classOf[JUnitRunner])
class TestReduce extends FunSuite with BeforeAndAfter {
  val q = new Query

  before {
    resetTmpToken()
  }

  test("-> .") {
    val goal = Atom(Word("late"), List(Word("Tom")))
    val rule = new Rule(Atom(Word("late"), List(Word("Tom"))))
    assert(q.reduce(goal, rule) == (True, Map()))
  }

  test("-> ?") {
    val goal = Atom(Word("late"), List(Word("Tom")))
    val rule = new Rule(Atom(Word("late"), List(Word("Bill"))))
    assert(q.reduce(goal, rule) == (False, Map()))
  }

  test("-> new goal") {
    val goal = Atom(Word("late"), List(Word("Tom")))
    val cond = Atom(Word("faulty"), List(Word("car")))
    val rule = new Rule(Atom(Word("late"), List(Word("Tom"))), cond)
    assert(q.reduce(goal, rule) == (cond, Map()))
  }

  test("replace one variable in rule") {
    val goal = Atom(Word("has-features"), List(Word("sparrow")))
    val rule = new Rule(Atom(Word("has-features"), List(Variable("X"))),
      Atom(Word("type-of"), List(Variable("X"), Word("bird"))))
    assert(q.reduce(goal, rule) == (Atom(Word("type-of"), List(Word("sparrow"), Word("bird"))),
      Map()))
  }

  test("replace one variable in goal") {
    val goal = Atom(Word("left-of"), List(Variable("X"), Word("bob")))
    val rule = new Rule(Atom(Word("left-of"), List(Word("jill"), Word("bob"))))
    assert(q.reduce(goal, rule) == (True, Map(Variable("X") -> Word("jill"))))
  }

  test("role variables bind to goal variables") {
    val goal = Atom(Word("retires"), List(Variable("Z")))
    val rule = new Rule(Atom(Word("retires"), List(Variable("X"))),
      Atom(Word("age"), List(Variable("X"), Integer(65))))
    assert(q.reduce(goal, rule) == (Atom(Word("age"), List(Variable("Z"), Integer(65))), Map()))
  }

  test("role variables bind to goal variables with replacement") {
    val goal = Atom(Word("costs"), List(Variable("Y"), Variable("X")))
    val rule = new Rule(Atom(Word("costs"), List(Word("fish"), Variable("Y"))),
      Atom(Word("sells"), List(Variable("Z"), Word("fish"), Variable("Y"))))
    val (g, s) = q.reduce(goal, rule)
    assert(g == Atom(Word("sells"), List(Variable("T_1"), Word("fish"), Variable("X"))))
    assert(s(Variable("Y")) == Word("fish"))
  }

  test("role variables unmatched with goal variables") {
    val goal = Atom(Word("costs"), List(Word("butter"), Variable("X")))
    val rule = new Rule(Atom(Word("costs"), List(Word("fish"), Variable("Y"))),
      Atom(Word("sells"), List(Variable("Z"), Word("fish"), Variable("Y"))))
    assert(q.reduce(goal, rule) == (False, Map()))
  }

  test("new goal is conjunctive") {
    val goal = Atom(Word("gives"), List(Variable("X"), Word("mary"), Variable("Z")))
    val rule = new Rule(
      Atom(Word("gives"), List(Word("santa"), Variable("Y1"), Variable("Y2"))),
      Conj(List(
        Atom(Word("asked-for"), List(Variable("Y1"), Variable("Y2"))),
        Atom(Word("deserves"), List(Variable("Y1"), Variable("Y2")))
      ))
    )
    assert(q.reduce(goal, rule) == (Conj(List(
      Atom(Word("asked-for"), List(Word("mary"), Variable("Z"))),
      Atom(Word("deserves"), List(Word("mary"), Variable("Z")))
    )), Map(Variable("X") -> Word("santa"))))
  }

  test("recursive reduction") {
    val goal = Atom(Word("member-of"), List(Integer(3), CList(
      List(Integer(1), Integer(2), Integer(3), Integer(4))
    )))
    val rule = new Rule(Atom(Word("member-of"), List(Variable("X"),
      PList(List(Any), Variable("Ys")))),
      Atom(Word("member-of"), List(Variable("X"), Variable("Ys"))))
    assert(q.reduce(goal, rule) == (Atom(Word("member-of"), List(Integer(3), CList(
      List(Integer(2), Integer(3), Integer(4))
    ))), Map()))
  }
}

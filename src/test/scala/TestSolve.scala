import Types._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestSolve extends FunSuite {
  val db = Map(
    
  )

  val q = new Query(db)

  test("no reduction: same") {
    val goal = Atom(Word("late"), List(Word("Tom")))
    val rule = new Rule(Atom(Word("late"), List(Word("Tom"))))
    assert(q.reduce(goal, rule) == (True, Map()))
  }

  test("no reduction: different") {
    val goal = Atom(Word("late"), List(Word("Tom")))
    val rule = new Rule(Atom(Word("late"), List(Word("Bill"))))
    assert(q.reduce(goal, rule) == (False, Map()))
  }

  test("q.reduce to if") {
    val goal = Atom(Word("late"), List(Word("Tom")))
    val cond = Atom(Word("faulty"), List(Word("car")))
    val rule = new Rule(Atom(Word("late"), List(Word("Tom"))), cond)
    assert(q.reduce(goal, rule) == (cond, Map()))
  }

  test("replace one variable") {
    val goal = Atom(Word("has-features"), List(Word("sparrow")))
    val rule = new Rule(Atom(Word("has-features"), List(Variable("X"))),
      Atom(Word("type-of"), List(Variable("X"), Word("bird"))))
    assert(q.reduce(goal, rule) == (Atom(Word("type-of"), List(Word("sparrow"), Word("bird"))),
      Map()))
  }

  test("q.reduce which-query") {
    val goal = Atom(Word("retires"), List(Variable("Z")))
    val rule = new Rule(Atom(Word("retires"), List(Variable("X"))),
      Atom(Word("age"), List(Variable("X"), Integer(65))))
    assert(q.reduce(goal, rule) == (Atom(Word("age"), List(Variable("Z"), Integer(65))), Map()))
  }

  test("fail which-query") {
    val goal = Atom(Word("costs"), List(Word("butter"), Variable("X")))
    val rule = new Rule(Atom(Word("costs"), List(Word("fish"), Variable("Y"))),
      Atom(Word("sells"), List(Variable("Z"), Word("fish"), Variable("Y"))))
    assert(q.reduce(goal, rule) == (False, Map()))
  }

  test("conjunctive rule") {
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
}

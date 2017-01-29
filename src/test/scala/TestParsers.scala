import Parsers._
import Types._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestParsers extends FunSuite {
  val tp = new TermParser

  def parseTerm(in: String): tp.ParseResult[Term] = tp.parse(tp.term, in)

  test("parse 0") {
    assert(parseTerm("0").get == Integer(0))
  }

  test("parse -0") {
    assert(parseTerm("-0").get == Integer(0))
  }

  test("parse 123") {
    assert(parseTerm("123").get == Integer(123))
  }

  test("parse -123") {
    assert(parseTerm("-123").get == Integer(-123))
  }

  test("parse -1x") {
    assert(parseTerm("-1x").get == Integer(-1))
  }

  test("parse c") {
    assert(parseTerm("c").get == Word("c"))
  }

  test("parse cat") {
    assert(parseTerm("cat").get == Word("cat"))
  }

  test("parse cat1") {
    assert(parseTerm("cat1").get == Word("cat1"))
  }

  test("parse constInt") {
    assert(parseTerm("constInt").get == Word("constInt"))
  }

  test("parse under_score") {
    assert(parseTerm("under_score").get == Word("under"))
  }

  test("parse hello world") {
    assert(parseTerm("hello world").get == Word("hello"))
  }

  test("parse is-ok") {
    assert(parseTerm("is-ok").get == Word("is-ok"))
  }

  test("parse X") {
    assert(parseTerm("X").get == Variable("X"))
  }

  test("parse X1") {
    assert(parseTerm("X1").get == Variable("X1"))
  }

  test("parse Xs") {
    assert(parseTerm("Xs").get == Variable("Xs"))
  }

  test("parse X-1") {
    assert(parseTerm("X-1").get == Variable("X-1"))
  }

  test("parse Xs_") {
    assert(parseTerm("Xs_").get == Variable("Xs"))
  }

  test("parse _") {
    assert(parseTerm("_").get == Any)
  }

  test("parse _1") {
    assert(parseTerm("_1").get == Any)
  }

  test("parse []") {
    assert(tp.parse(tp.clist, "[]").get == CList())
  }

  test("parse [1]") {
    assert(parseTerm("[1]").get == intList(1))
  }


  test("parse [1,2,3]") {
    assert(parseTerm("[1,2,3]").get == intList(List(1, 2, 3)))
  }

  test("parse [[], [X], [1]]") {
    assert(parseTerm("[[], [X], [1]]").get == CList(List(
      CList(), CList(List(Variable("X"))), intList(1)
    )))
  }

  test("parse (X:XS)") {
    assert(parseTerm("(X:XS)").get == PList(List(Variable("X")), Variable("XS")))
  }

  test("parse (_:-1:2:X)") {
    assert(parseTerm("(_:-1:2:X)").get == PList(List(
      Any, Integer(-1), Integer(2)
    ), Variable("X")))
  }

  test("parse (X:-1:2:_)") {
    assert(parseTerm("(X:-1:2:_)").get == PList(List(
      Variable("X"), Integer(-1), Integer(2)
    ), Any))
  }

  test("parse ([]:X)") {
    assert(parseTerm("([]:X)").get == PList(List(CList()), Variable("X")))
  }

  val pp = new PredicateParser

  def parsePredicate(in: String): pp.ParseResult[Predicate] = pp.parse(pp.predicates, in)

  test("parse ok") {
    assert(parsePredicate("ok").get == Atom(Word("ok")))
  }

  test("parse is-animal(cat)") {
    assert(parsePredicate("is-animal(cat)").get == Atom(Word("is-animal"), List(Word("cat"))))
  }

  test("parse gives(X, Y, cat)") {
    assert(parsePredicate("gives(X, Y, cat)").get == Atom(Word("gives"), List(
      Variable("X"), Variable("Y"), Word("cat")
    )))
  }

  test("parse length-is([1,2,3], X)") {
    assert(parsePredicate("length-is([1,2,3], X)").get == Atom(Word("length-is"), List(
      intList(List(1, 2, 3)), Variable("X")
    )))
  }

  test("parse ~gives(X, _, cat)") {
    assert(parsePredicate("~gives(X, _, cat)").get == Not(Atom(Word("gives"), List(
      Variable("X"), Any, Word("cat")
    ))))
  }

  test("parse asked-for(Y1, Y2), deserves(Y1, Y2)") {
    assert(parsePredicate("asked-for(Y1, Y2), deserves(Y1, Y2)").get == Conj(List(
      Atom(Word("asked-for"), List(Variable("Y1"), Variable("Y2"))),
      Atom(Word("deserves"), List(Variable("Y1"), Variable("Y2")))
    )))
  }

  test("parse gives(X, _, cat).") {
    assert(parseQuery("gives(X, _, cat).").get == Expr(Atom(Word("gives"), List(
      Variable("X"), Any, Word("cat")
    ))))
  }

  test("parse ~gives(X, _, cat).") {
    assert(parseQuery("~gives(X, _, cat).").get == Expr(Not(Atom(Word("gives"), List(
      Variable("X"), Any, Word("cat")
    )))))
  }

  test("parse asked-for(Y1, Y2), deserves(Y1, Y2).") {
    assert(parseQuery("asked-for(Y1, Y2), deserves(Y1, Y2).").get == Expr(Conj(List(
      Atom(Word("asked-for"), List(Variable("Y1"), Variable("Y2"))),
      Atom(Word("deserves"), List(Variable("Y1"), Variable("Y2")))
    ))))
  }

  test("parse :load list") {
    assert(parseQuery(":load list").get == Command("load", List("list")))
  }

  test("parse :load /home/dbs/db1") {
    assert(parseQuery(":load /home/dbs/db1").get == Command("load", List("/home/dbs/db1")))
  }

  test("parse :q") {
    assert(parseQuery(":q").get == Command("q"))
  }

  test("parse a database") {
    val expected = new Database(List(
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

    val content =
      """            test.
                     left-of(jill, bob).
                     left-of(bob, tony).
                     left-of(tony, tracey).
                     left-of(tracey, ian).
                     left-of(ian, mary).
                     left-of(mary, sam).
                     mutters(jill).
                     mutters(mary).
                     cheers(bob).
                     cheers(tracey).
                     cheers(ian).
                     dozes(tony).
                     dozes(sam).
                     beside(X, Y) :- left-of(X, Y).
                     beside(X, Y) :- left-of(Y, X).
                     happy(X) :- cheers(X).
                     disturbed-by(X, Y) :- dozes(X), beside(X, Y), cheers(Y).
                     is-a-thing(X)."""

    assert(parseRules(content).get.toString == expected.toString)
  }

}
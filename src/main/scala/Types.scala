/**
  * Created by paul on 22/01/2017.
  */
object Types {
  // substitute the first term (Variable) as the second term
  // e.g. Variable("x") = Word("hello")
  type Sub = Map[Term, Term]

  abstract class Term {
    def substituteWith(sub: Sub): Term = this
  }

  case class Word(const: String) extends Term

  case class Variable(name: String) extends Term {
    override def substituteWith(sub: Sub): Term = sub.get(this) match {
      case Some(t) => t
      case None => this
    }
  }

  case object Any extends Term

  case class Integer(value: Int) extends Term

  case class TermList(terms: List[Term]) extends Term

  abstract class Predicate {
    def substituteWith(sub: Sub): Predicate = this
  }

  case object True extends Predicate

  case object False extends Predicate

  case class Atom(verb: Word, args: List[Term]) extends Predicate {
    def this(verb: Word) = this(verb, Nil)

    override def substituteWith(sub: Sub): Atom = Atom(verb, args.map(_.substituteWith(sub)))
  }

  case class Not(atom: Atom) extends Predicate {
    override def substituteWith(sub: Sub): Not = Not(atom.substituteWith(sub))
  }

  case class Conj(preds: List[Predicate]) extends Predicate {
    override def substituteWith(sub: Sub): Conj = Conj(preds.map(_.substituteWith(sub)))
  }

  class Rule(val rear: Atom, val front: Predicate) {
    def this(rear: Atom) = this(rear, True)
  }

  type Sig = (Word, Int)

  type Database = Map[Sig, List[Rule]]
}

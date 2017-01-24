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

  case class Word(const: String) extends Term {
    override def toString: String = const
  }

  case class Variable(name: String) extends Term {
    override def substituteWith(sub: Sub): Term = sub.get(this) match {
      case Some(t) => t
      case None => this
    }

    override def toString: String = name
  }

  case object Any extends Term {
    override def toString: String = "_"
  }

  case class Integer(value: Int) extends Term {
    override def toString: String = s"$value"
  }

  case class TermList(terms: List[Term]) extends Term

  abstract class Predicate {
    def substituteWith(sub: Sub): Predicate = this
  }

  abstract class Boolean extends Predicate

  case object True extends Boolean {
    override def toString: String = "."
  }

  case object False extends Boolean {
    override def toString: String = "?"
  }

  case class Atom(verb: Word, args: List[Term] = Nil) extends Predicate {
    override def substituteWith(sub: Sub): Atom = Atom(verb, args.map(_.substituteWith(sub)))

    override def toString: String = s"$verb${
      if (args.isEmpty) ""
      else args.mkString(", ")
    }"
  }

  case class Not(atom: Atom) extends Predicate {
    override def substituteWith(sub: Sub): Not = Not(atom.substituteWith(sub))

    override def toString: String = s"~$atom"
  }

  case class Conj(preds: List[Predicate]) extends Predicate {
    override def substituteWith(sub: Sub): Conj = Conj(preds.map(_.substituteWith(sub)))

    override def toString: String = preds.mkString(" & ")
  }

  case class Rule(rear: Atom, front: Predicate = True, id: Int = 0) {
    override def toString: String = front match {
      case True => s"$rear"
      case _ => s"$rear :- $front"
    }
  }

  case class Sig(name: Word, dim: Int) {
    override def toString: String = s"$name/$dim"
  }

  type Database = Map[Sig, List[Rule]]
}

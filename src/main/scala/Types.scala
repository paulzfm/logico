/**
  * Created by paul on 22/01/2017.
  */

object Types {

  /**
    * Substitutions of form like `Variable(X) -> Int(1)`.
    */
  type Sub = Map[Term, Term]

  abstract class Term {
    def substituteWith(sub: Sub): Term = this

    def substituteWith(key: Variable, value: Term): Term = substituteWith(Map(key -> value))

    def variableToLowerCase: Term = this

    def variableToUpperCase: Term = this
  }

  case class Word(const: String) extends Term {
    override def toString: String = const
  }

  case class Variable(name: String) extends Term {
    override def substituteWith(sub: Sub): Term = sub.get(this) match {
      case Some(t) => t
      case None => this
    }

    override def variableToLowerCase: Term = Variable(name.toLowerCase)

    override def variableToUpperCase: Term = Variable(name.toUpperCase)

    override def toString: String = name
  }

  case object Any extends Term {
    override def toString: String = "_"
  }

  case class Integer(value: Int) extends Term {
    override def toString: String = s"$value"
  }

  /**
    * Complete list (we know the length).
    * For example, [X, Y, cat, 123].
    * Especially, `CList(Nil)` is the empty list.
    *
    * @param terms all terms of list.
    */
  case class CList(terms: List[Term] = Nil) extends Term {
    def split(headLength: Int): List[Term] = {
      val (head, tail) = terms.splitAt(headLength)
      head :+ CList(tail)
    }

    override def substituteWith(sub: Sub): Term = CList(terms.map(_.substituteWith(sub)))

    override def variableToLowerCase: Term = CList(terms.map(_.variableToLowerCase))

    override def variableToUpperCase: Term = CList(terms.map(_.variableToUpperCase))

    override def toString: String = s"[${terms.mkString(", ")}]"
  }

  /**
    * Partial list (we do not know the length).
    * The whole list is composed of `head` and `tail`.
    * For example, X : Y : Z, where [X, Y] is `head` and `Z` denotes `tail` list.
    *
    * @param head first part of the list.
    * @param tail denotes the second (remaining) part.
    */
  case class PList(head: List[Term], tail: Term) extends Term {
    def split(headLength: Int): List[Term] = {
      val (head1, tail1) = head.splitAt(headLength)
      head1 :+ PList(tail1, tail)
    }

    override def substituteWith(sub: Sub): Term = sub.get(tail) match {
      case None => PList(head.map(_.substituteWith(sub)), tail)
      case Some(CList(ts)) => CList(head.map(_.substituteWith(sub)) ++ ts)
      case Some(PList(ts, v)) => PList(head.map(_.substituteWith(sub)) ++ ts, v)
      case Some(t) => CList(head.map(_.substituteWith(sub)) :+ t)
    }

    override def variableToLowerCase: Term = PList(head.map(_.variableToLowerCase),
      tail.variableToLowerCase)

    override def variableToUpperCase: Term = PList(head.map(_.variableToUpperCase),
      tail.variableToUpperCase)

    override def toString: String = s"${head.mkString(":")}:$tail"
  }

  abstract class Predicate {
    def substituteWith(sub: Sub): Predicate = this

    def variableToLowerCase: Predicate = this

    def variableToUpperCase: Predicate = this
  }

  abstract class Bool extends Predicate

  case object True extends Bool {
    override def toString: String = "."
  }

  case object False extends Bool {
    override def toString: String = "?"
  }

  case class Atom(verb: Word, args: List[Term] = Nil) extends Predicate {
    override def substituteWith(sub: Sub): Atom = Atom(verb, args.map(_.substituteWith(sub)))

    override def variableToLowerCase: Atom = Atom(verb, args.map(_.variableToLowerCase))

    override def variableToUpperCase: Atom = Atom(verb, args.map(_.variableToUpperCase))

    override def toString: String = s"$verb${
      if (args.isEmpty) ""
      else s"(${args.mkString(", ")})"
    }"
  }

  case class Not(atom: Atom) extends Predicate {
    override def substituteWith(sub: Sub): Not = Not(atom.substituteWith(sub))

    override def variableToLowerCase: Predicate = Not(atom.variableToLowerCase)

    override def variableToUpperCase: Predicate = Not(atom.variableToUpperCase)

    override def toString: String = s"~$atom"
  }

  case class Conj(preds: List[Predicate]) extends Predicate {
    override def substituteWith(sub: Sub): Conj = Conj(preds.map(_.substituteWith(sub)))

    override def variableToLowerCase: Predicate = Conj(preds.map(_.variableToLowerCase))

    override def variableToUpperCase: Predicate = Conj(preds.map(_.variableToUpperCase))

    override def toString: String = preds.mkString(" & ")
  }

  class Rule(rearPart: Atom, frontPart: Predicate = True) {
    val rear: Atom = rearPart.variableToLowerCase

    val front: Predicate = frontPart.variableToLowerCase

    override def toString: String = frontPart match {
      case True => s"$rearPart."
      case _ => s"$rearPart :- $frontPart."
    }

    lazy val sig: Sig = Sig(rearPart.verb, rearPart.args.length)
  }

  case class Sig(name: Word, dim: Int) {
    override def toString: String = s"$name/$dim"
  }

  class Database(val rules: List[Rule] = Nil) {
    private val rulesWithId: List[(Int, Rule)] = (1 to rules.length).toList.zip(rules)

    private val hashMap: Map[Sig, List[(Int, Rule)]] = rulesWithId.groupBy(_._2.sig)

    def get(sig: Sig): Option[List[(Int, Rule)]] = hashMap.get(sig)

    def append(newRules: List[Rule]): Database = new Database(rules ++ newRules)

    override def toString: String = rulesWithId map {
      case (id, rule) => s"($id) $rule\n"
    } mkString
  }

}

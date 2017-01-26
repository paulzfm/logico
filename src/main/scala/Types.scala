/**
  * Created by paul on 22/01/2017.
  */

import RandomTokens.getTmpToken

object Types {

  /**
    * Substitutions of form like `Variable(X) -> Int(1)`.
    */
  type Sub = Map[Variable, Term]

  def wrapper1[T](xs: List[Term], sub: Sub, cons: List[Term] => T): Option[T] = {
    val newTerms = xs.flatMap(_.substituteWith(sub))
    if (newTerms.length != xs.length) None
    else Some(cons(newTerms))
  }

  def wrapper2[T](xs: List[Term], sub: Sub, cons: List[Term] => Option[T]): Option[T] = {
    val newTerms = xs.flatMap(_.substituteWith(sub))
    if (newTerms.length != xs.length) None
    else cons(newTerms)
  }

  def wrapper3[T](xs: List[Predicate], sub: Sub, cons: List[Predicate] => T): Option[T] = {
    val newPreds = xs.flatMap(_.substituteWith(sub))
    if (newPreds.length != xs.length) None
    else Some(cons(newPreds))
  }

  val hasGoalStyle: Variable => Boolean = {
    case Variable(name) => name.charAt(0).isUpper
  }

  val hasRuleStyle: Variable => Boolean = {
    case Variable(name) => name.charAt(0).isLower
  }

  val hasOriginalGoalStyle: Variable => Boolean = {
    case Variable(name) => name.charAt(0).isUpper &&
      (if (name.length > 1) name.charAt(1) != '_' else true)
  }

  abstract class Term {
    def substituteWith(sub: Sub): Option[Term] = Some(this)

    def collectVariables(p: (Variable) => Boolean): Set[Variable] = Set()
  }

  case class Word(const: String) extends Term {
    override def toString: String = const
  }

  case class Variable(name: String) extends Term {
    override def substituteWith(sub: Sub): Option[Term] = sub.get(this) match {
      case Some(t) => Some(t)
      case None => Some(this)
    }

    override def collectVariables(p: (Variable) => Boolean): Set[Variable] =
      if (p(this)) Set(this) else Set()

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

    override def substituteWith(sub: Sub): Option[Term] =
      wrapper1(terms, sub, CList)

    override def collectVariables(p: (Variable) => Boolean): Set[Variable] =
      terms.flatMap(_.collectVariables(p)).toSet

    override def toString: String = s"[${terms.mkString(", ")}]"
  }

  /**
    * Partial list (we do not know the length).
    * The whole list is composed of `head` and `tail`.
    * For example, X : Y : Z, where [X, Y] is `head` and `Z` denotes `tail` list.
    *
    * @param head first part of the list.
    * @param tail Variable | Any, denotes the second (remaining) part.
    */
  case class PList(head: List[Term], tail: Term) extends Term {
    def split(headLength: Int): List[Term] = {
      val (head1, tail1) = head.splitAt(headLength)
      head1 :+ PList(tail1, tail)
    }

    override def substituteWith(sub: Sub): Option[Term] = {
      val newHead = head.flatMap(_.substituteWith(sub))
      if (newHead.length != head.length) None
      else tail match {
        case Variable(v1) => sub.get(Variable(v1)) match {
          case None => Some(PList(newHead, tail))
          case Some(Variable(v)) => Some(PList(newHead, Variable(v)))
          case Some(CList(ts)) => Some(CList(newHead ++ ts))
          case Some(PList(ts, v)) => Some(PList(newHead ++ ts, v))
          case _ => None
        }
        case Any => Some(PList(newHead, tail))
      }
    }

    override def collectVariables(p: (Variable) => Boolean): Set[Variable] =
      head.flatMap(_.collectVariables(p)) ++ (tail match {
        case Variable(v) => List(Variable(v))
        case _ => Nil
      }) toSet

    override def toString: String = s"${head.mkString(":")}:$tail"
  }

  abstract class Predicate {
    def substituteWith(sub: Sub): Option[Predicate] = Some(this)

    def collectVariables(p: (Variable) => Boolean): Set[Variable] = Set()

    def variableToRuleStyle: Predicate = {
      val vs = collectVariables(hasGoalStyle).toList
      val sub: Sub = vs.map {
        case Variable(v) => (Variable(v), Variable(v.toLowerCase))
      }.toMap
      substituteWith(sub) match {
        case Some(p) => p
      }
    }

    def variableToGoalStyle: Predicate = {
      val vs = collectVariables(hasRuleStyle).toList
      val sub: Sub = vs.map {
        case Variable(v) => (Variable(v), Variable(getTmpToken))
      }.toMap
      substituteWith(sub) match {
        case Some(p) => p
      }
    }
  }

  abstract class Bool extends Predicate

  case object True extends Bool {
    override def toString: String = "."
  }

  case object False extends Bool {
    override def toString: String = "?"
  }

  case class Atom(verb: Word, args: List[Term] = Nil) extends Predicate {
    override def substituteWith(sub: Sub): Option[Atom] =
      wrapper1(args, sub, Atom(verb, _))

    override def collectVariables(p: (Variable) => Boolean): Set[Variable] =
      args.flatMap(_.collectVariables(p)).toSet

    override def toString: String = s"$verb${
      if (args.isEmpty) ""
      else s"(${args.mkString(", ")})"
    }"
  }

  case class Not(atom: Atom) extends Predicate {
    override def substituteWith(sub: Sub): Option[Predicate] =
      atom.substituteWith(sub) match {
        case Some(a) => Some(Not(a))
        case None => None
      }

    override def collectVariables(p: (Variable) => Boolean): Set[Variable] =
      atom.collectVariables(p)

    override def toString: String = s"~$atom"
  }

  case class Conj(preds: List[Predicate]) extends Predicate {
    override def substituteWith(sub: Sub): Option[Predicate] = {
      val newPreds = preds.flatMap(_.substituteWith(sub))
      if (newPreds.length != preds.length) None
      else Some(Conj(newPreds))
    }

    override def collectVariables(p: (Variable) => Boolean): Set[Variable] =
      preds.flatMap(_.collectVariables(p)).toSet

    override def toString: String = preds.mkString(" & ")
  }

  class Rule(rearPart: Atom, frontPart: Predicate = True) {
    val rear: Predicate = rearPart.variableToRuleStyle

    val front: Predicate = frontPart.variableToRuleStyle

    override def toString: String = frontPart match {
      case True => s"$rearPart."
      case _ => s"$rearPart :- $frontPart."
    }

    lazy val sig: Sig = Sig(rearPart.verb, rearPart.args.length)

    lazy val args: List[Term] = rear match {
      case Atom(_, as) => as
    }
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

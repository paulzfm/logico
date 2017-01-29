/**
  * Created by paul on 22/01/2017.
  *
  * Types.
  *
  * Type definitions of Term and Predicate.
  */

import RandomTokens.getTmpToken

object Types {

  /**
    * Substitutions of form like `Variable(X) -> Int(1)`. When means we can replace `Variable(X)`
    * with value `Int(1)`.
    */
  type Sub = Map[Variable, Term]

  def showSub(sub: Sub): String = sub map {
    case (t1, t2) => s"$t1 = $t2"
  } mkString ", "

  /**
    * Substitute each of terms `xs` with `sub`, if any one fails (yields `None`), the whole
    * substitution fails.
    * Otherwise, we get the new terms `xs'` and then we call the user defined function `cons` to
    * construct an object of type `T` and returns `Some(T)`.
    *
    * I.e., this helper function makes it easier to substitute many terms and wrap it into some
    * target object of type `T`.
    *
    * @param xs   terms.
    * @param sub  argument of `substituteWith`. The semantics of `substituteWith` will be
    *             discussed later in class `Term`.
    * @param cons object constructor.
    * @tparam T object type.
    * @return - `Some(obj)` if all substitutions succeed.
    *         - `None` if fails.
    */
  def wrapper1[T](xs: List[Term], sub: Sub, cons: List[Term] => T): Option[T] = {
    val newTerms = xs.flatMap(_.substituteWith(sub))
    if (newTerms.length != xs.length) None
    else Some(cons(newTerms))
  }

  /**
    * Same as `wrapper1` except that the constructor `cons` returns the final result.
    *
    * @param xs   terms.
    * @param sub  argument of `substituteWith`.
    * @param cons object constructor but returns `Option[T]` instead of `T`.
    * @tparam T object type.
    * @return same as `wrapper1`.
    */
  def wrapper2[T](xs: List[Term], sub: Sub, cons: List[Term] => Option[T]): Option[T] = {
    val newTerms = xs.flatMap(_.substituteWith(sub))
    if (newTerms.length != xs.length) None
    else cons(newTerms)
  }

  /**
    * Same as `wrapper1` except that `xs` is a list of `Predicate`, not `Term`.
    *
    * @param xs   predicates.
    * @param sub  argument of `substituteWith`.
    * @param cons object constructor.
    * @tparam T object type.
    * @return same as `wrapper1`.
    */
  def wrapper3[T](xs: List[Predicate], sub: Sub, cons: List[Predicate] => T): Option[T] = {
    val newPreds = xs.flatMap(_.substituteWith(sub))
    if (newPreds.length != xs.length) None
    else Some(cons(newPreds))
  }

  /**
    * Any user input variable must start with a UPPER-CASE letter.
    * But to tell goal variables (variables inside a goal) and rule variables (variables inside a
    * rule), we have to make them different.
    *
    * A goal variable still must start with a UPPER-CASE letter.
    *
    */
  val hasGoalStyle: Variable => Boolean = {
    case Variable(name) => name.charAt(0).isUpper
  }

  /**
    * To contrast, a rule variable must start with a lower-case letter.
    */
  val hasRuleStyle: Variable => Boolean = {
    case Variable(name) => name.charAt(0).isLower
  }

  /**
    * Since the front part of a rule can be reduced into the new goal. We have to rename these
    * rule variables in goal style. To avoid name conflicts with the original goal variables,
    * which are defined by the user, all these introduced temporary variables have form `T_<int>`
    * (as shown in `RandomTokens`).
    *
    * This predicate is used to tell the original goal variables (which contains no `_`)
    * from the temporary variables and rule variables.
    */
  val hasOriginalGoalStyle: Variable => Boolean = {
    case Variable(name) => name.charAt(0).isUpper &&
      (if (name.length > 1) name.charAt(1) != '_' else true)
  }

  /**
    * A predicate is composed of many terms.
    */
  abstract class Term {
    /**
      * Substitute a term with `sub`. That is to say, to replace any variables in term with the
      * corresponding value stored in `sub`.
      *
      * @param sub substitution mappings.
      * @return - `Some(t)` when no conflicts happen and `t` is the new term.
      *         - `None` if any conflict happens.
      */
    def substituteWith(sub: Sub): Option[Term] = Some(this)

    /**
      * Collect all variables which satisfy the predicate `p` inside this term.
      *
      * @param p the predicate (filter).
      * @return All variables satisfing `p`.
      */
    def collectVariables(p: (Variable) => Boolean): Set[Variable] = Set()
  }

  /**
    * A string as a constant value.
    *
    * @param const the string.
    */
  case class Word(const: String) extends Term {
    override def toString: String = const
  }

  /**
    * A variable. Either in rule style or in goal style.
    *
    * @param name variable name.
    */
  case class Variable(name: String) extends Term {
    override def substituteWith(sub: Sub): Option[Term] = sub.get(this) match {
      case Some(t) => Some(t)
      case None => Some(this)
    }

    override def collectVariables(p: (Variable) => Boolean): Set[Variable] =
      if (p(this)) Set(this) else Set()

    override def toString: String = name
  }

  /**
    * A wildcard that matches any term.
    */
  case object Any extends Term {
    override def toString: String = "_"
  }

  /**
    * A constant integer value.
    *
    * @param value the integer value.
    */
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

  def intList(int: Int): CList = intList(List(int))

  def intList(ints: List[Int]): CList = CList(ints.map(Integer))

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

  /**
    * Predicate either occurs in the query or the rule/fact.
    */
  abstract class Predicate {
    /**
      * Substitute a predicate with `sub`. That is to say, to replace any variables in predicate
      * with the corresponding value stored in `sub`.
      *
      * @param sub substitution mappings.
      * @return - `Some(t)` when no conflicts happen and `t` is the new term.
      *         - `None` if any conflict happens.
      */
    def substituteWith(sub: Sub): Option[Predicate] = Some(this)

    /**
      * Collect all variables which satisfy the predicate `p` inside this predicate.
      *
      * @param p the predicate (filter).
      * @return All variables satisfing `p`.
      */
    def collectVariables(p: (Variable) => Boolean): Set[Variable] = Set()

    /**
      * Replace all variables into rule style (lower-case).
      */
    def variableToRuleStyle: Predicate = {
      val vs = collectVariables(hasGoalStyle).toList
      val sub: Sub = vs.map {
        case Variable(v) => (Variable(v), Variable(v.toLowerCase))
      }.toMap
      substituteWith(sub) match {
        case Some(p) => p
      }
    }

    /**
      * Replace all rule variables into goal style (upper-case) by introducing temporary variables.
      */
    def variableToGoalStyle: (Predicate, Sub) = {
      val vs = collectVariables(hasRuleStyle).toList
      val sub: Sub = vs.map {
        case Variable(v) => (Variable(v), Variable(getTmpToken))
      }.toMap
      substituteWith(sub) match {
        case Some(p) => (p, sub)
      }
    }
  }

  /**
    * True (no more goals to solve) or False (goals cannot be reached).
    */
  abstract class Bool extends Predicate

  case object True extends Bool {
    override def toString: String = "."
  }

  case object False extends Bool {
    override def toString: String = "?"
  }

  /**
    * A atomic predicate of form `verb(args)`.
    *
    * @param verb the name.
    * @param args arguments. NOTE zero argument is allowed.
    */
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

  /**
    * Negation of the `atom`.
    *
    * @param atom the Atomic predicate.
    */
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

  /**
    * A conjuction of many predicates `preds`.
    *
    * @param preds predicates.
    */
  case class Conj(preds: List[Predicate]) extends Predicate {
    override def substituteWith(sub: Sub): Option[Predicate] = {
      val newPreds = preds.flatMap(_.substituteWith(sub))
      if (newPreds.length != preds.length) None
      else Some(Conj(newPreds))
    }

    override def collectVariables(p: (Variable) => Boolean): Set[Variable] =
      preds.flatMap(_.collectVariables(p)).toSet

    override def toString: String = preds.mkString(", ")
  }

  /**
    * A rule, read as `rearPart if frontPart`.
    * Or a fact when `frontPart` is true, read as `rearPart`.
    *
    * NOTE internally, we represent all rule variables in rule style. So we change them into rule
    * style once created it since user defined rules can only have variables in goal style.
    *
    * A function can be defined with more than one rules. To reduce with this function, we try
    * the rules in the defined order.
    *
    * @param rearPart  the conclusion.
    * @param frontPart the condition.
    */
  class Rule(rearPart: Atom, frontPart: Predicate = True) {
    val rear: Predicate = rearPart.variableToRuleStyle

    val front: Predicate = frontPart.variableToRuleStyle

    override def toString: String = frontPart match {
      case True => s"$rearPart."
      case _ => s"$rearPart :- $frontPart."
    }

    /**
      * The signature.
      */
    lazy val sig: Sig = Sig(rearPart.verb, rearPart.args.length)

    lazy val args: List[Term] = rear match {
      case Atom(_, as) => as
    }
  }

  /**
    * Signature of a function.
    *
    * @param name function name.
    * @param dim  the number of arguments it expects.
    *             NOTE that rules with the SAME name but DIFFERENT number of arguments are
    *             DIFFERENT functions.
    */
  case class Sig(name: Word, dim: Int) {
    override def toString: String = s"$name/$dim"
  }
}

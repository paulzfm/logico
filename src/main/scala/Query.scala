/**
  * Created by paul on 22/01/2017.
  */

import Types._

object Query {
  /**
    * Tell whether two terms can be made equal (matched).
    *
    * @param goal   term inside the goal
    * @param origin term inside the rule
    * @return - Some(s1, s2) if matched, and `s1` is the substitutions for goal variables,
    *         `s2` is the substitutions for rule variables
    *         - None if not matched
    */
  def matchTerms(goal: Term, origin: Term): Option[(Sub, Sub)] = (goal, origin) match {
    case (Word(c1), Word(c2)) if c1 == c2 => Some((Map(), Map()))
    case (Integer(n1), Integer(n2)) if n1 == n2 => Some((Map(), Map()))
    case (Variable(v), Word(_)) => Some((Map(goal -> origin), Map()))
    case (Variable(v), Integer(_)) => Some((Map(goal -> origin), Map()))
    case (_, Variable(v)) => Some((Map(), Map(origin -> goal)))
    case (Any, _) => Some((Map(), Map()))
    case (_, Any) => Some((Map(), Map()))
    case _ => None
  }

  // ASSUME same signature
  def reduce(goal: Atom, rule: Rule): (Predicate, Sub) = {
    def loop(goalTerms: List[Term], ruleTerms: List[Term],
             goalSub: Sub, ruleSub: Sub): Option[(Sub, Sub)] =
      (goalTerms, ruleTerms) match {
        case (Nil, Nil) => Some((goalSub, ruleSub))
        case (t1 :: xs, t2 :: ys) => matchTerms(t1, t2) match {
          case None => None
          case Some((s1, s2)) => loop(xs.map(_.substituteWith(s1)), ys.map(_.substituteWith(s2)),
            goalSub ++ s1, ruleSub ++ s2)
        }
        case _ => None
      }

    loop(goal.args, rule.rear.args, Map(), Map()) match {
      case None => (False, Map())
      case Some((s1, s2)) => (rule.front.substituteWith(s2), s1)
    }
  }

  def solve(goal: Predicate): Option[List[Sub]] = ???

}

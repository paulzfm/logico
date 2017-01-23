/**
  * Created by paul on 22/01/2017.
  */

import Types._

class Query(val db: Database = Map()) {

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

  /**
    * Reduce a `goal` with a given `rule`.
    *
    * @param goal goal of type Atom
    * @param rule the given rule
    * @return (p, s) where p is the new goal to be reduces (if p = False, we cannot continue)
    *         and s is the substitutions for goal variables
    */
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

  /**
    * Try to reduce the `goal` with given `rules`.
    *
    * @param goal  goal of type Atom
    * @param rules rules to be tried
    * @param acc   solutions found so far
    * @return same as `solve`
    */
  def solveAtomWithRules(goal: Atom, rules: List[Rule], acc: List[Sub]): Option[List[Sub]] = rules
  match {
    case Nil => acc match {
      case Nil => None // no solutions ever found
      case _ => Some(acc) // found at least one solution
    }
    case r :: rs =>
      val (g, s) = reduce(goal, r)
      solve(g) match {
        case Some(ss) => solveAtomWithRules(goal, rs, acc ++ ss.map(_ ++ s)) // find a solution
        case None => solveAtomWithRules(goal, rs, acc) // ignore this failed trail
      }
  }

  /**
    * Try to find a trace that reaches all the `goals` given.
    *
    * @param goals goal list
    * @param acc   solutions found so far
    * @return same as `solve`
    */
  def solveGoals(goals: List[Predicate], acc: List[Sub]): Option[List[Sub]] = goals
  match {
    case Nil => acc match {
      case Nil => None // no solutions ever found
      case _ => Some(acc) // found at least one solution
    }
    case g :: gs => solve(g) match {
      case Some(ss) => (for {
        s <- ss
        xs <- solveGoals(gs.map(_.substituteWith(s)), acc :+ s)
      } yield xs).flatten match {
        case Nil => None // the remaining goals can never reached
        case x => Some(x)
      }
      case None => None // when any one of the goals fails, the conjunctive goal fails
    }
  }

  /**
    * Solve `goal` with database `db`.
    *
    * @param goal query, either is-query (without variables) or which-query (with variables)
    * @return - Some(subs) if succeed, where `subs` are all substitutions for each possible
    *         reduction trace, each substitution (represent a solution) can be empty if it is a
    *         is-query
    *         - None if fail
    */
  def solve(goal: Predicate): Option[List[Sub]] = goal match {
    case True => Some(List(Map())) // simplest goal
    case False => None // failure
    case Atom(v, as) => db.get((v, as.length)) match {
      case Some(rs) => solveAtomWithRules(Atom(v, as), rs, Nil)
      case None => None // no such signature
    }
    case Not(a) => solve(a) // solve the negation
    match {
      case Some(_) => None
      case None => Some(List(Map())) // no substitutions found
    }
    case Conj(ps) => solveGoals(ps, Nil)
  }

}

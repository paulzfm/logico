/**
  * Created by paul on 22/01/2017.
  */

import BuiltIns.bf
import RandomTokens.resetTmpToken
import TraceTree._
import Types._

/**
  * Solving strategies.
  *
  * @param db database that we base all queries on.
  */
class Solver(val db: Database = new Database) {

  /**
    * Tell whether two terms can be made equal (matched).
    *
    * @param goal   term inside the goal.
    * @param origin term inside the rule.
    * @return - `Some(s)` if matched. `s` is the substitutions for both goal and rule variables.
    *         NOTE all goal variables start with *UPPER CASE* and rule variables start with *lower
    *         case*.
    *         - `None` if not matched.
    */
  def matchTerms(goal: Term, origin: Term): Option[Sub] = (goal, origin) match {
    case (Any, _) => Some(Map())
    case (_, Any) => Some(Map())
    case (Variable(v1), Variable(v2)) if v1 == v2 => Some(Map())
    case (_, Variable(v)) => Some(Map(Variable(v) -> goal))
    case (Variable(v), _) => Some(Map(Variable(v) -> origin))
    case (Word(c1), Word(c2)) if c1 == c2 => Some(Map())
    case (Integer(n1), Integer(n2)) if n1 == n2 => Some(Map())
    case (CList(ts1), CList(ts2)) if ts1.length == ts2.length =>
      matchTermLists(ts1, ts2, Map())
    case (CList(ts1), PList(ts2, v)) if ts1.length >= ts2.length =>
      matchTermLists(CList(ts1).split(ts2.length), ts2 :+ v, Map())
    case (PList(ts1, v), CList(ts2)) if ts1.length <= ts2.length =>
      matchTermLists(ts1 :+ v, CList(ts2).split(ts1.length), Map())
    case (PList(ts1, v1), PList(ts2, v2)) =>
      if (ts1.length >= ts2.length)
        matchTermLists(PList(ts1, v1).split(ts2.length), ts2 :+ v2, Map())
      else matchTermLists(ts1 :+ v1, PList(ts2, v2).split(ts1.length), Map())
    case _ => None
  }

  /**
    * Tell whether two term lists can be made equal (matched).
    *
    * @param goalTerms terms inside goal.
    * @param ruleTerms terms inside rule.
    * @param sub       substitutions for goal and rule variables so far.
    * @return the same as `matchTerms`.
    */
  def matchTermLists(goalTerms: List[Term], ruleTerms: List[Term], sub: Sub): Option[Sub] =
    (goalTerms, ruleTerms) match {
      case (Nil, _) => Some(sub)
      case (_, Nil) => Some(sub)
      case (t1 :: ts1, t2 :: ts2) => matchTerms(t1, t2) match {
        case None => None // when any element fails, the list term fails
        case Some(s) =>
          merge(sub, s) match {
            case Some(newSub) => wrapper2(ts1, newSub, newGoalTerms =>
              wrapper2(ts2, newSub, newRuleTerms =>
                matchTermLists(newGoalTerms, newRuleTerms, newSub)
              )
            )
            case None => None
          }
      }
    }

  /**
    * Merge `sub1` with new generated substitutions `sub2`.
    * To merge means to union and simplify: replace variables with already solved values.
    *
    * @param sub1 old substitutions.
    * @param sub2 new substitutions generated.
    * @return the same as `matchTerms`.
    */
  def merge(sub1: Sub, sub2: Sub, includeSub2: Boolean = true): Option[Sub] =
    wrapper1(sub1.values.toList, sub2, ts => {
      val s1 = sub1.keys.zip(ts).toMap
      if (includeSub2) s1 ++ sub2 else s1
    })

  /**
    * Reduce a `goal` with a given `rule`.
    * ASSUME that `goal` and `rule` already have the same signature.
    *
    * @param goal goal to be reduced.
    * @param rule rule to be applied.
    * @return (`pred`, `sub`) where
    *         - `pred` is the new goal we are going to reduce next. NOTE if `pred` is `False`, we
    *         cannot continue due to the contradiction.
    *         - `sub` is the substitutions introduced when applying.
    */
  def reduce(goal: Atom, rule: Rule): (Predicate, Sub) =
    matchTermLists(goal.args, rule.args, Map()) match {
      case None => (False, Map())
      case Some(s) =>
        val s1 = s.filterKeys {
          case Variable(v) if hasGoalStyle(Variable(v)) => true
          case _ => false
        }
        val s2 = s.filterKeys {
          case Variable(v) if hasRuleStyle(Variable(v)) => true
          case _ => false
        }
        rule.front.substituteWith(s2) match {
          case Some(g) =>
            val (newGoal, s3) = g.variableToGoalStyle
            merge(s1, s3) match {
              case Some(newSub) => (newGoal, newSub)
              case None => (False, Map())
            }
          case None => (False, Map())
        }
    }

  /**
    * Solve `goal` with given `rules`.
    *
    * @param goal      goal to be reached.
    * @param rules     remaining rules to be tried.
    * @param solutions solutions found so far.
    * @param children  children created for the RNode so far.
    * @return same as `solve`.
    */
  def solveAtomWithRules(goal: Atom, rules: List[(Int, Rule)], solutions: List[Sub],
                         children: List[(Int, Sub, TTree)]): (List[Sub], TTree) = rules match {
    case Nil => (solutions, RNode(goal, children))
    case (id, r) :: rs =>
      val (newGoal, introducedSub) = reduce(goal, r)
      val (results, trace) = solveEntry(newGoal)
      solveAtomWithRules(goal, rs, solutions ++ results.flatMap(merge(introducedSub, _)),
        children :+ (id, introducedSub, trace))
  }

  /**
    * Solve more than one `goals`.
    *
    * @param goals remaining goals to be reached.
    * @param dep   dependent substitutions. For example, to solve `g1 & g2`, when `g1` introduces
    *              some new substitutions `s`, before solving `g2`, we should first substitute `g2`
    *              with `s`, which yields `g2'`, and we continue solving this `g2'`. All these new
    *              introduced substitutions must be stored in `dep` as the dependence for the
    *              remaining `goals`.
    * @return same as `solve`.
    */
  def solveGoals(goals: List[Predicate], dep: Sub): (List[Sub], TTree) = goals match {
    case Nil => throw new Exception("goals cannot be empty")
    case g :: Nil => solveEntry(g) // one goal left
    case g :: gs =>
      val (results, trace) = solveEntry(g)
      if (results.isEmpty) (Nil, trace) // when any goal fails, the conjunctive goal fails
      else {
        val (allResults, allTraces) = (for {
          result <- results
          newDep <- merge(dep, result)
          (newResult, newTrace) <- wrapper3(gs, result, solveGoals(_, newDep))
        } yield (newResult.flatMap(merge(newDep, _)), newTrace)).unzip
        (allResults.flatten, trace.appendGoals(allTraces)._1)
      }
  }

  /**
    * Solve `goal` with database `db`.
    *
    * @param goal Query, either is-query (without variables) or which-query (with variables).
    * @return (`results`, `trace`) where `trace` records the search paths and `results` has length
    *         - 0 if we fail to reach this `goal`.
    *         - >= 1 if we succeed to reach this `goal` with some substitutions. Since we can have
    *         more than one solutions, we record all of them as a list `results`, each representing
    *         one possible replacement for variables shown in the query.
    *         NOTE that `results` should contain an empty substitution for successful is-query
    *         since no variables need to be replaced.
    */
  def solveEntry(goal: Predicate): (List[Sub], TTree) = goal match {
    case True => (List(Map()), accepted) // simplest goal
    case False => (Nil, rejected) // failure
    case Atom(v, as) =>
      val sig = Sig(v, as.length)
      // first try built-in functions
      bf.get(sig) match {
        case Some(f) =>
          val (results, tree) = f(as)
          (results,
            if (results.isEmpty) RNode(Atom(v, as), List((0, Map(), tree)))
            else RNode(Atom(v, as), List((0, results.head, tree)))
          )
        case None =>
          // then search rules in database
          db.get(Sig(v, as.length)) match {
            case Some(rs) => solveAtomWithRules(Atom(v, as), rs, Nil, Nil)
            case None => (Nil, RNode(Atom(v, as))) // no such signature
          }
      }
    case Not(a) =>
      val (results, trace) = solveEntry(a) // solve the negation
      results match {
        case Nil => (List(Map()), NNode(Not(a), trace, True)) // no substitutions found if succeed
        case _ => (Nil, NNode(Not(a), trace, False))
      }
    case Conj(ps) =>
      val (subs, trace) = solveGoals(ps, Map())
      (subs, SNode(Conj(ps), trace))
  }

  /**
    * Public interface you should call to solve a query.
    * Compared with `solveEntry`, we filter `solveEntry`'s results to make it contain only the
    * original goal variables.
    *
    * @param goal same as `solveEntry`.
    * @return same as `solveEntry`, except that we do some filtering job.
    */
  def solve(goal: Predicate): (List[Sub], TTree) = {
    resetTmpToken()
    val (results, tree) = solveEntry(goal)
    System.err.println(results)
    (results.map(_.filterKeys(hasOriginalGoalStyle)), tree)
  }

}

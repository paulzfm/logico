/**
  * Created by paul on 22/01/2017.
  */

import Types._

class Query(val db: Database = new Database) {

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
    case (_, Variable(_)) => Some(Map(origin -> goal))
    case (Variable(_), _) => Some(Map(goal -> origin))
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
          val newSub = simplify(sub, s.toList)
          matchTermLists(ts1.map(_.substituteWith(newSub)), ts2.map(_.substituteWith(newSub)),
            newSub)
      }
    }

  def simplify(sub: Sub, subs: List[(Term, Term)]): Sub = subs match {
    case Nil => sub
    case (Variable(v1), Variable(v2)) :: ss =>
      simplify(sub.updated(Variable(v1), Variable(v2)), ss)
    case (Variable(v), t) :: ss =>
      val newSub = sub.map {
        case (v1, Variable(v2)) if v2 == v => (v1, t)
        case other => other
      }
      simplify(newSub.updated(Variable(v), t), ss)
    case (t1, t2) :: _ => throw new Exception(s"illegal substitution: $t1 = $t2")
  }

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
    matchTermLists(goal.args, rule.rear.args, Map()) match {
      case None => (False, Map())
      case Some(s) =>
        val s1 = s.filterKeys {
          case Variable(v) if v.head.isUpper => true
          case _ => false
        }
        val s2 = s.filterKeys {
          case Variable(v) if v.head.isLower => true
          case _ => false
        }
        (rule.front.substituteWith(s2), s1)
    }

  /**
    * Trace tree: to record the search traces.
    */
  abstract class TTree {
    def appendGoals(trees: List[TTree]): (TTree, List[TTree])

    def toLines(indent: Int = 0): List[(Int, String)]

    override def toString: String = toLines().map {
      case (indent, str) => s"${"  " * indent}<-- $str\n"
    } mkString
  }

  /**
    * Leaf node: cannot be reduced any more, but a new goal can be appended to it as its child.
    */
  case class Leaf(goal: Bool, child: Option[TTree] = None) extends TTree {
    override def appendGoals(trees: List[TTree]): (TTree, List[TTree]) = goal match {
      case True => (Leaf(True, Some(trees.head)), trees.tail)
      case False => (this, trees)
    }

    override def toLines(indent: Int = 0): List[(Int, String)] = {
      List((indent, s"$goal")) ++ (child match {
        case Some(t) => t.toLines(indent + 1)
        case None => Nil
      })
    }
  }

  val accepted: TTree = Leaf(True)

  val rejected: TTree = Leaf(False)

  /**
    * Reduce node: a Atom `goal` can be reduced to other goals by applying rules.
    *
    * @param goal     the Atom goal.
    * @param children each in form of (`id`, `sub`, `node`) where
    *                 - `id` is the ID of the rule applied;
    *                 - `sub` is the substitutions introduced when applying;
    *                 - `node` stores the new goal.
    *                 NOTE we always append a `rejected` leaf node as the last children, which means
    *                 we have no more rules to reach the goal.
    */
  case class RNode(goal: Atom, children: List[(Int, Sub, TTree)] = Nil) extends TTree {
    override def appendGoals(trees: List[TTree]): (TTree, List[TTree]) = {
      def loop(cs: List[TTree], ts: List[TTree],
               acc: List[TTree]): (List[TTree], List[TTree]) = cs match {
        case Nil => (acc, ts)
        case c :: xs =>
          val (t1, ts1) = c.appendGoals(ts)
          loop(xs, ts1, acc :+ t1)
      }

      val (cs, ts) = loop(children.unzip3._3, trees, Nil)
      (RNode(goal, children.zip(cs).map {
        case ((id, sub, _), t) => (id, sub, t)
      }), ts)
    }

    override def toLines(indent: Int): List[(Int, String)] = (indent, s"$goal") ::
      children.flatMap {
        case (id, sub, tree) => tree.toLines(indent + 1) match {
          case Nil => throw new Exception("lines cannot be empty")
          case (i, s) :: ls => (i, s"$s ($id) ${
            if (sub.nonEmpty) s"[${showSub(sub)}]"
            else ""
          }") :: ls
        }
      }

    private def showSub(sub: Sub): String = sub map {
      case (t1, t2) => s"$t1 = $t2"
    } mkString ", "
  }

  /**
    * Not node: a Not goal which we first solve the negation and then reverse the solution.
    *
    * @param goal     the Not goal.
    * @param child    the negation of `goal`.
    * @param accepted whether `goal` is reached or not finally.
    */
  case class NNode(goal: Not, child: TTree, accepted: Bool) extends TTree {
    override def appendGoals(trees: List[TTree]): (TTree, List[TTree]) = child.appendGoals(trees)

    override def toLines(indent: Int): List[(Int, String)] = ((indent, s"$goal") ::
      child.toLines(indent + 1)) :+ (indent, s"$accepted")
  }

  /**
    * Spilt node: a Conj goal containing more than one sub-goals, we should solve the first goal
    * above all.
    *
    * @param goal  the Conj goal.
    * @param child the first sub-goal of `goal`.
    */
  case class SNode(goal: Conj, child: TTree) extends TTree {
    override def appendGoals(trees: List[TTree]): (TTree, List[TTree]) = child.appendGoals(trees)

    override def toLines(indent: Int): List[(Int, String)] =
      (indent, s"$goal") :: child.toLines(indent + 1)
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
      val (results, trace) = solve(newGoal)
      solveAtomWithRules(goal, rs, solutions ++ results.map(introducedSub ++ _),
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
    case g :: Nil => solve(g) // one goal left
    case g :: gs =>
      val (results, trace) = solve(g)
      if (results.isEmpty) (Nil, trace) // when any goal fails, the conjunctive goal fails
      else {
        val (allResults, allTraces) = (for {
          result <- results
          newDep = dep ++ result
          (newResult, newTrace) = solveGoals(gs.map(_.substituteWith(result)), newDep)
        } yield (newResult.map(_ ++ newDep), newTrace)).unzip
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
  def solve(goal: Predicate): (List[Sub], TTree) = goal match {
    case True => (List(Map()), accepted) // simplest goal
    case False => (Nil, rejected) // failure
    case Atom(v, as) => db.get(Sig(v, as.length)) match {
      case Some(rs) => solveAtomWithRules(Atom(v, as), rs, Nil, Nil)
      case None => (Nil, RNode(Atom(v, as))) // no such signature
    }
    case Not(a) =>
      val (results, trace) = solve(a) // solve the negation
      results match {
        case Nil => (List(Map()), NNode(Not(a), trace, True)) // no substitutions found if succeed
        case _ => (Nil, NNode(Not(a), trace, False))
      }
    case Conj(ps) =>
      val (subs, trace) = solveGoals(ps, Map())
      (subs, SNode(Conj(ps), trace))
  }

}

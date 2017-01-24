/**
  * Created by paul on 22/01/2017.
  */

import Types.{Sub, _}

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

  // Trace tree: to record the search traces.
  abstract class TTree {
    def appendGoals(trees: List[TTree]): (TTree, List[TTree])

    def toLines(indent: Int = 0): List[(Int, String)]
  }

  // Leaf node: cannot be reduced any more, but a new goal can be appended as its child.
  case class Leaf(goal: Boolean, child: Option[TTree] = None) extends TTree {
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

  // Reduce node: a Atom goal can be reduced to other goals by applying rules.
  // we store the edges in form of (id, sub, node) where
  // - id is the ID of the rule applied
  // - sub is the substitutions generated when applying
  // - node stores the new goal
  // NOTE we always append a `rejected` leaf node as the last children, which means we have
  // no more rules to reach the goal.
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

    override def toLines(indent: Int) = (indent, s"$goal") :: children.flatMap {
      case (id, sub, tree) => tree.toLines(indent + 1) match {
        case (i, s) :: ls => (i, s"$s ($id) ${
          if (sub.nonEmpty) s"[$sub]"
          else ""
        }") :: ls
      }
    }
  }

  // Not node: a Not goal which we first solve the negation goal and then reverse the solution.
  case class NNode(goal: Not, child: TTree, accepted: Boolean) extends TTree {
    override def appendGoals(trees: List[TTree]): (TTree, List[TTree]) = child.appendGoals(trees)

    override def toLines(indent: Int) = ((indent, s"$goal") ::
      child.toLines(indent + 1)) :+ (indent, s"$accepted")
  }

  // Spilt node: a Conj goal containing more than one sub-goals, we should solve the first goal
  // above all.
  case class SNode(goal: Conj, child: TTree) extends TTree {
    override def appendGoals(trees: List[TTree]): (TTree, List[TTree]) = child.appendGoals(trees)

    override def toLines(indent: Int) = (indent, s"$goal") :: child.toLines(indent + 1)
  }

  /**
    * Try to reduce the `goal` with given `rules`.
    *
    * @param goal  goal of type Atom
    * @param rules rules to be tried
    * @param acc   solutions found so far
    * @return same as `solve`
    */
  def solveAtomWithRules(goal: Atom, rules: List[Rule], acc: List[Sub],
                         cs: List[(Int, Sub, TTree)]): (List[Sub], TTree) = rules match {
    case Nil => (acc, RNode(goal, cs))
    case r :: rs =>
      val (g, s) = reduce(goal, r)
      val (re, t) = solve(g)
      solveAtomWithRules(goal, rs, acc ++ re, cs :+ (r.id, s, t))
  }

  /**
    * Try to find a trace that reaches all the `goals` given.
    *
    * @param goals goal list
    * @return same as `solve`
    */
  def solveGoals(goals: List[Predicate], dep: Sub): (List[Sub],
    TTree) = goals
  match {
    case g :: Nil => solve(g)
    case g :: gs =>
      val (subs, t) = solve(g)
      if (subs.isEmpty) (Nil, t) // any goal fails, the conjunctive goal fails
      else {
        val (l1, l2) = (for {
          s <- subs
          d = dep ++ s
          (ans, tr) = solveGoals(gs.map(_.substituteWith(s)), d)
        } yield (ans.map(_ ++ d), tr)).unzip
        (l1.flatten, t.appendGoals(l2)._1)
      }
  }

  /**
    * Solve `goal` with database `db`.
    *
    * @param goal query, either is-query (without variables) or which-query (with variables)
    * @return (result, trace) where trace has value iff `trace` is enabled and result is
    *         - Some(subs) if succeed, where `subs` are all substitutions for each possible
    *         reduction trace, each substitution (represent a solution) can be empty if it is a
    *         is-query
    *         - None if fail
    */
  def solve(goal: Predicate): (List[Sub], TTree) = goal match {
    case True => (List(Map()), accepted) // simplest goal
    case False => (Nil, rejected) // failure
    case Atom(v, as) => db.get(Sig(v, as.length)) match {
      case Some(rs) => solveAtomWithRules(Atom(v, as), rs, Nil, Nil)
      case None => (Nil, RNode(Atom(v, as))) // no such signature
    }
    case Not(a) =>
      val (r, t) = solve(a) // solve the negation
      r match {
        case Nil => (List(Map()), NNode(Not(a), t, True)) // no substitutions found if succeed
        case _ => (Nil, NNode(Not(a), t, False))
      }
    case Conj(ps) =>
      val (subs, tree) = solveGoals(ps, Map())
      (subs, SNode(Conj(ps), tree))
  }

}

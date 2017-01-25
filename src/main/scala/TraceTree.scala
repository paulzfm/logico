/**
  * Created by paul on 26/01/2017.
  */
import Types._

object TraceTree {
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
}

/**
  * Created by paul on 28/01/2017.
  */

import Types._

import scala.util.parsing.combinator._

object Parsers {

  class TermParser extends RegexParsers {
    def integer: Parser[Integer] =
      """(0|(-)?[1-9][0-9]*)""".r ^^ { str =>
        Integer(str.toInt)
      }

    def word: Parser[Word] =
      """[a-z][-A-Za-z0-9]*""".r ^^ Word

    def variable: Parser[Variable] =
      """[A-Z][-A-Za-z0-9]*""".r ^^ Variable

    def wildCard: Parser[Term] = "_" ^^ { _ => Any }

    def clist: Parser[CList] = literal("[") ~ repsep(term, ",") ~ literal("]") ^^ {
      case _ ~ terms ~ _ => CList(terms)
    }

    def plist: Parser[PList] = rep1sep(term, ":") ^^ { terms =>
      terms.last match {
        case Variable(v) => PList(terms.init, Variable(v))
        case Any => PList(terms.init, Any)
      }
    } | literal("(") ~ plist ~ literal(")") ^^ {
      case _ ~ list ~ _ => list
    }

    def term: Parser[Term] = integer | word | variable | wildCard | clist | plist
  }

  class PredicateParser extends TermParser {
    def atom: Parser[Atom] = word ^^ { verb =>
      Atom(verb)
    } | word ~ literal("(") ~ rep1sep(term, ",") ~ literal(")") ^^ {
      case verb ~ _ ~ args ~ _ => Atom(verb, args)
    }

    def not: Parser[Not] = literal("~") ~ atom ^^ {
      case _ ~ atom => Not(atom)
    }

    def conj: Parser[Conj] = rep1sep(predicate, "&") ^^ { preds =>
      Conj(preds)
    }

    def predicate: Parser[Predicate] = atom | not | conj
  }

  class RuleParser extends PredicateParser {
    def rule: Parser[Rule] = atom ~ opt(predicate) ~ literal(".") ^^ {
      case rear ~ None ~ _ => new Rule(rear)
      case rear ~ Some(front) ~ _ => new Rule(rear, front)
    }
  }

  class DatabaseParser extends RuleParser {
    def database: Parser[Database] = rep(rule) ^^ { rules =>
      new Database(rules)
    }
  }

  abstract class Command

  case class Query(pred: Predicate) extends Command

  case class Load(file: String) extends Command

  case object Exit extends Command

  class CommandParser extends PredicateParser {
    def query: Parser[Query] = predicate ^^ Query

    def exit: Parser[Command] = literal("q") ^^ { _ => Exit } |
      literal("quit") ^^ { _ => Exit } |
      literal("exit") ^^ { _ => Exit }

    def command: Parser[Command] = query | exit
  }

}
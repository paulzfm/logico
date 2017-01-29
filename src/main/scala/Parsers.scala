/**
  * Created by paul on 28/01/2017.
  */

import Types._

import scala.util.parsing.combinator._

object Parsers {

  class TermParser extends RegexParsers {
    def integer: Parser[Integer] =
      """-?(0|[1-9][0-9]*)""".r ^^ { str =>
        Integer(str.toInt)
      }

    def word: Parser[Word] =
      """[a-z][-A-Za-z0-9]*""".r ^^ Word

    def variable: Parser[Variable] =
      """[A-Z][-A-Za-z0-9]*""".r ^^ Variable

    def any: Parser[Term] = "_" ^^ { _ => Any }

    def clist: Parser[CList] = "[" ~ repsep(term, ",") ~ "]" ^^ {
      case _ ~ terms ~ _ => CList(terms)
    }

    def plist: Parser[PList] = "(" ~ rep1sep(term, ":") ~ ")" ^^ {
      case _ ~ terms ~ _ => terms.last match {
        case Variable(v) => PList(terms.init, Variable(v))
        case Any => PList(terms.init, Any)
      }
    }

    def term: Parser[Term] = integer | word | variable | any | clist | plist
  }

  class PredicateParser extends TermParser {
    def atom: Parser[Atom] = word ~ opt("(" ~ rep1sep(term, ",") ~ ")") ^^ {
      case verb ~ None => Atom(verb)
      case verb ~ Some(_ ~ args ~ _) => Atom(verb, args)
    }

    def not: Parser[Not] = "~" ~ atom ^^ {
      case _ ~ atom => Not(atom)
    }

    def predicate: Parser[Predicate] = atom | not

    def predicates: Parser[Predicate] = repsep(predicate, ",") ^^ {
      case p :: Nil => p
      case ps => Conj(ps)
    }
  }

  class RuleParser extends PredicateParser {
    def rule: Parser[Rule] = atom ~ opt(":-" ~ predicates) ~ "." ^^ {
      case rear ~ None ~ _ => new Rule(rear)
      case rear ~ Some(_ ~ front) ~ _ => new Rule(rear, front)
    }
  }

  class DatabaseParser extends RuleParser {
    def database: Parser[Database] = phrase(rep(rule)) ^^ { rules =>
      new Database(rules)
    }
  }

  abstract class Query

  case class Expr(pred: Predicate) extends Query

  case class Command(op: String, args: List[String] = Nil) extends Query

  class QueryParser extends PredicateParser {
    def expr: Parser[Expr] = predicates ~ "." ^^ {
      case pred ~ _ => Expr(pred)
    }

    // TODO a better regex
    def ident: Parser[String] =
      """[^ ]+""".r

    def command: Parser[Command] = ":" ~ word ~ rep(ident) ^^ {
      case _ ~ Word(op) ~ args => Command(op, args)
    }

    def query: Parser[Query] = phrase(command | expr)
  }

  val dp = new DatabaseParser

  def parseRules(rules: String): dp.ParseResult[Database] = dp.parse(dp.database, rules)

  val qp = new QueryParser

  def parseQuery(query: String): qp.ParseResult[Query] = qp.parse(qp.query, query)

}
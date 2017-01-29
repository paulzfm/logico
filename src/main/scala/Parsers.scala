/**
  * Created by paul on 28/01/2017.
  *
  * Parsers.
  *
  * To parse queries and rules into AST types defined in `Types.scala`.
  */

import Types._

import scala.util.parsing.combinator._

object Parsers {

  /**
    * Parse terms.
    */
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

    def plist: Parser[PList] = "(" ~ rep(term ~ ":") ~ (any | variable) ~ ")" ^^ {
      case _ ~ ts ~ last ~ _ =>
        val terms = ts map {
          case t ~ _ => t
        }
        PList(terms, last)
    }

    def term: Parser[Term] = integer | word | variable | any | clist | plist
  }

  /**
    * Parse predicates.
    */
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
      case Nil => True
      case p :: Nil => p
      case ps => Conj(ps)
    }
  }

  /**
    * Parse rules.
    */
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

  /**
    * Query user inputs from interactive console.
    */
  abstract class Query

  /**
    * Expression shown as is-query or which-query. Must stop with a `.`.
    *
    * @param pred the predicate.
    */
  case class Expr(pred: Predicate) extends Query

  /**
    * Commands supported by REPL.
    *
    * @param op   command name (operator name).
    * @param args arguments (if any).
    */
  case class Command(op: String, args: List[String] = Nil) extends Query

  /**
    * Parse query.
    */
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

  /**
    * Parse a string interpreted as rules into a `Database`.
    *
    * @param rules input string.
    * @return database.
    */
  def parseRules(rules: String): dp.ParseResult[Database] = dp.parse(dp.database, rules)

  val qp = new QueryParser

  /**
    * Parse a string interpreted as a query into a `Query`.
    *
    * @param query query string.
    * @return query.
    */
  def parseQuery(query: String): qp.ParseResult[Query] = qp.parse(qp.query, query)

}
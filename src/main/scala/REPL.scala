/**
  * Created by paul on 29/01/2017.
  */

import Parsers._
import Types.{Sub, showSub}

import scala.io.Source.fromFile
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

class REPL(db: Database = new Database) {
  private var _db: Database = db
  private var _solver: Solver = new Solver(_db)
  private var showTrace: Boolean = false

  def printInfo(): Unit = {
    println("Welcome to logico, a mini PROLOG-like logic programming language!")
    println()
    println("For help, use `:h'. To quit, type `:q'.")
    println()
  }

  def printPrompt(): Unit = print("?-> ")

  def printContinuePrompt(): Unit = print(" -> ")

  def printHelp(): Unit = {
    def fill(targetLength: Int): String => String =
      str => s"$str${" " * (targetLength - str.length)}"

    def printTable(left: List[String], right: List[String]): Unit = {
      val lf = fill(left.map(_.length).max + 4)
      val rf = fill(right.map(_.length).max + 4)
      left.zip(right).foreach {
        case (l, r) => println("    " + lf(l) + rf(r))
      }
    }

    println("Commands")
    println()
    printTable(List(
      ":h|:help",
      ":q|:quit|:exit",
      ":show-db",
      ":load <path>",
      ":set-trace on|off"
    ), List(
      "print help",
      "exit",
      "print database rules",
      "load database file from <path>",
      "whether to show search trace"
    ))
    println()
  }

  def readInput(acc: String = ""): String = {
    val line = readLine().trim
    if (line.charAt(0) == ':' || line.last == '.') acc + line
    else {
      printContinuePrompt()
      readInput(acc + line)
    }
  }

  def printSolutions(solutions: List[Sub]): Unit = solutions match {
    case Nil => println("false.")
    case sub :: Nil =>
      if (sub.isEmpty) println("true.")
      else println(showSub(sub) + ".")
    case ss => println(ss.map(showSub).mkString(";\n") + ".")
  }

  def executeCommand(op: String, args: List[String]): Unit = op match {
    case "h" | "help" => printHelp()
    case "q" | "quit" | "exit" =>
      println("Bye!")
      System.exit(0)
    case "show-db" => print(_db)
    case "load" =>
      if (args.nonEmpty) load(args.head)
      else throw new MissingArgumentsError("load", 1)
    case "set-trace" =>
      if (args.nonEmpty) args.head match {
        case "on" => showTrace = true
        case "off" => showTrace = false
        case other => throw new InvalidArgumentsError(other, "`on' or `off'")
      }
      else throw new MissingArgumentsError("set-trace", 1)
    case _ => throw new NoSuchCommandError(op)
  }

  def load(dbPath: String): Unit = {
    Try(fromFile(dbPath)) match {
      case Success(lines) =>
        parseRules(lines.mkString) match {
          case rp.Success(rules, _) =>
            _db = _db.append(rules)
            _solver = new Solver(_db)
            println("true.")
          case rp.Failure(msg, next) =>
            Console.err.println("Parsing ERROR: " + msg + ":")
            Console.err.println(next.pos.longString)
          case rp.Error(msg, next) =>
            Console.err.println("ERROR: " + msg + ":")
            Console.err.println(next.pos.longString)
        }
      case Failure(ex) => Console.err.println(ex)
    }
  }

  def launch(): Unit = {
    printInfo()
    while (true) {
      printPrompt()
      val in = readInput()
      parseQuery(in) match {
        case qp.Success(Expr(pred), _) =>
          Try(_solver.solve(pred)) match {
            case Success((results, trace)) =>
              printSolutions(results)
              if (showTrace) println(trace)
            case Failure(ex) => Console.err.println(ex.getMessage)
          }
        case qp.Success(Command(op, args), _) =>
          Try(executeCommand(op, args)) match {
            case Success(_) => println("true.")
            case Failure(ex) => Console.err.println(ex.getMessage)
          }
        case qp.Failure(msg, next) =>
          Console.err.println("Parsing ERROR: " + msg + ":")
          Console.err.println(next.pos.longString)
        case qp.Error(msg, next) =>
          Console.err.println("ERROR: " + msg)
          Console.err.println(next.pos.longString)
      }
    }
  }
}

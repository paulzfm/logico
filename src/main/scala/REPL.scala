/**
  * Created by paul on 29/01/2017.
  */

import Parsers._
import Types.{Sub, showSub}
import scala.io.Source.fromFile
import scala.util.{Try, Success, Failure}

import scala.io.StdIn.readLine

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

  def printHelp(): Unit = println("help")

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
    case "showdb" => print(_db)
    case "load" =>
      if (args.length > 0) load(args.head)
      else Console.err.println("Missing argument for load.")
    case _ => Console.err.println(s"Unknown command: $op.")
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
          val (results, trace) = _solver.solve(pred)
          printSolutions(results)
          if (showTrace) println(trace)
        case qp.Success(Command(op, args), _) =>
          executeCommand(op, args)
        case qp.Failure(msg, next) =>
          Console.err.println("Parsing ERROR: " + msg + ":")
          Console.err.println(next.pos.longString)
        case qp.Error(msg, _) => Console.err.println("ERROR: " + msg)
      }
    }
  }
}

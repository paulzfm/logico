/**
  * Created by paul on 29/01/2017.
  */

import Parsers._
import Types.{Database, Sub, showSub}

import scala.io.StdIn.readLine

class REPL(db: Database = new Database) {
  private var _db: Database = db
  private var _solver: Solver = new Solver(_db)
  private var showTrace: Boolean = false

  def printInfo(): Unit = println("welcome")

  def printPrompt(): Unit = print("?-> ")

  def printContinuePrompt(): Unit = print(" -> ")

  def printHelp(): Unit = println("help")

  def readInput(acc: String = ""): String = {
    val line = readLine().trim
    if (line.charAt(0) == ':' || line.last == '.') acc ++ line
    else {
      printContinuePrompt()
      readInput(acc ++ line)
    }
  }

  def printSolutions(solutions: List[Sub]): Unit =
    println(solutions.map(showSub).mkString(";\n") ++ ".")

  def executeCommand(op: String, args: List[String]): Unit = printHelp()

  def load(dbPath: String): Unit = ???

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
        case qp.Success(Command(op, args), _) => executeCommand(op, args)
        case qp.Failure(msg, _) => Console.err.println("FAILURE: " + msg)
        case qp.Error(msg, _) => Console.err.println("ERROR: " + msg)
      }
    }
  }
}

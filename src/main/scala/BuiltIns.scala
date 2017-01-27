/**
  * Created by paul on 26/01/2017.
  */

import TraceTree._
import Types._

import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

object BuiltIns {
  /**
    * Built-in functions type.
    */
  type Func = List[Term] => (List[Sub], TTree)

  val bf: Map[Sig, Func] = Map(
    Sig(Word("sum"), 3) -> {
      case Integer(n1) :: Integer(n2) :: Integer(n3) :: Nil =>
        if (n1 + n2 == n3) (List(Map()), accepted)
        else (Nil, rejected)
      case Variable(v) :: Integer(n2) :: Integer(n3) :: Nil =>
        (List(Map(Variable(v) -> Integer(n3 - n2))), accepted)
      case Integer(n1) :: Variable(v) :: Integer(n3) :: Nil =>
        (List(Map(Variable(v) -> Integer(n3 - n1))), accepted)
      case Integer(n1) :: Integer(n2) :: Variable(v) :: Nil =>
        (List(Map(Variable(v) -> Integer(n1 + n2))), accepted)
      case as =>
        throw new Exception(s"insufficient arguments: $as")
    },

    Sig(Word("times"), 3) -> {
      case Integer(n1) :: Integer(n2) :: Integer(n3) :: Nil =>
        if (n1 * n2 == n3) (List(Map()), accepted)
        else (Nil, rejected)
      case Variable(v) :: Integer(n2) :: Integer(n3) :: Nil =>
        (List(Map(Variable(v) -> Integer(n3 / n2))), accepted)
      case Integer(n1) :: Variable(v) :: Integer(n3) :: Nil =>
        (List(Map(Variable(v) -> Integer(n3 / n1))), accepted)
      case Integer(n1) :: Integer(n2) :: Variable(v) :: Nil =>
        (List(Map(Variable(v) -> Integer(n1 * n2))), accepted)
      case _ => throw new Exception("insufficient arguments")
    },

    Sig(Word("lt"), 2) -> {
      case Integer(n1) :: Integer(n2) :: Nil =>
        if (n1 < n2) (List(Map()), accepted)
        else (Nil, rejected)
      case _ => throw new Exception("insufficient arguments")
    },

    Sig(Word("eq"), 2) -> {
      case t1 :: t2 :: Nil =>
        if (t1 == t2) (List(Map()), accepted)
        else (Nil, rejected)
      case _ => throw new Exception("insufficient arguments")
    },

    Sig(Word("print"), 1) -> {
      case Word(str) :: Nil =>
        print(str)
        (List(Map()), accepted)
    },

    Sig(Word("read"), 1) -> {
      case Variable(v) :: Nil => (List(Map(Variable(v) -> Word(readLine()))), accepted)
    },

    Sig(Word("int"), 2) -> {
      case Word(str) :: Variable(v) :: Nil =>
        Try(str.toInt) match {
          case Success(i) => (List(Map(Variable(v) -> Integer(i))), accepted)
          case Failure(_) => (Nil, rejected)
        }
    }
  )
}

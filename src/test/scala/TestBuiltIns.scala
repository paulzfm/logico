import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import java.nio.charset.StandardCharsets

import Types._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestBuiltIns extends FunSuite {
  val q = new Query

  test("1 + 11 = 12") {
    val (results, _) = q.solve(Atom(Word("sum"),
      List(Integer(1), Integer(11), Integer(12))))
    assert(results == List(Map()))
  }

  test("1 + 11 = X") {
    val (results, _) = q.solve(Atom(Word("sum"),
      List(Integer(1), Integer(11), Variable("X"))))
    assert(results == List(Map(Variable("X") -> Integer(12))))
  }

  test("X + 11 = 12") {
    val (results, _) = q.solve(Atom(Word("sum"),
      List(Variable("X"), Integer(11), Integer(12))))
    assert(results == List(Map(Variable("X") -> Integer(1))))
  }

  test("1 + X = 12") {
    val (results, _) = q.solve(Atom(Word("sum"),
      List(Integer(1), Variable("X"), Integer(12))))
    assert(results == List(Map(Variable("X") -> Integer(11))))
  }

  test("2 * 3 = 6") {
    val (results, _) = q.solve(Atom(Word("times"),
      List(Integer(2), Integer(3), Integer(6))))
    assert(results == List(Map()))
  }

  test("2 * 3 = X") {
    val (results, _) = q.solve(Atom(Word("times"),
      List(Integer(2), Integer(3), Variable("X"))))
    assert(results == List(Map(Variable("X") -> Integer(6))))
  }

  test("X * 3 = 6") {
    val (results, _) = q.solve(Atom(Word("times"),
      List(Variable("X"), Integer(3), Integer(6))))
    assert(results == List(Map(Variable("X") -> Integer(2))))
  }

  test("2 * X = 6") {
    val (results, trace) = q.solve(Atom(Word("times"),
      List(Integer(2), Variable("X"), Integer(6))))
    println(trace)
    assert(results == List(Map(Variable("X") -> Integer(3))))
  }

  test("3 < 2 ?") {
    val (results, _) = q.solve(Atom(Word("lt"), List(Integer(3), Integer(2))))
    assert(results.isEmpty)
  }

  test("3 < 3 ?") {
    val (results, _) = q.solve(Atom(Word("lt"), List(Integer(3), Integer(3))))
    assert(results.isEmpty)
  }

  test("3 < 4") {
    val (results, _) = q.solve(Atom(Word("lt"), List(Integer(3), Integer(4))))
    assert(results == List(Map()))
  }

  test("2 = 2") {
    val (results, _) = q.solve(Atom(Word("eq"), List(Integer(2), Integer(2))))
    assert(results == List(Map()))
  }

  test("2 /= 3") {
    val (results, _) = q.solve(Atom(Word("eq"), List(Integer(2), Integer(3))))
    assert(results.isEmpty)
  }

  test("cat = cat") {
    val (results, _) = q.solve(Atom(Word("eq"), List(Word("cat"), Word("cat"))))
    assert(results == List(Map()))
  }

  test("[1,2,3] = [1,2,3]") {
    val (results, _) = q.solve(Atom(Word("eq"), List(
      CList(List(Integer(1), Integer(2), Integer(3))),
      CList(List(Integer(1), Integer(2), Integer(3)))
    )))
    assert(results == List(Map()))
  }

  test("print \"Hello, world!\"") {
    val out: OutputStream = new ByteArrayOutputStream
    Console.withOut(out) {
      val str = "Hello, world!"
      val (results, _) = q.solve(Atom(Word("print"), List(Word(str))))
      assert(results == List(Map()))
      out.flush()
      assert(out.toString == str)
    }
  }

  test("read X") {
    val str = "Hello, world!"
    val in: InputStream = new ByteArrayInputStream(str.getBytes(StandardCharsets.UTF_8))
    Console.withIn(in) {
      val (results, _) = q.solve(Atom(Word("read"), List(Variable("X"))))
      assert(results == List(Map(Variable("X") -> Word(str))))
    }
  }

  test("int \"12\"") {
    val (results, _) = q.solve(Atom(Word("int"), List(Word("12"), Variable("X"))))
    assert(results == List(Map(Variable("X") -> Integer(12))))
  }

  test("int \"12 \"") {
    val (results, _) = q.solve(Atom(Word("int"), List(Word("12 "), Variable("X"))))
    assert(results.isEmpty)
  }
}
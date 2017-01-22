import Types._
import Query._

val a1 =
val r1 = new Rule(a1, Atom(Word("faulty"), List(Word("car"))))
val g1 = a1

reduce(g1, r1)
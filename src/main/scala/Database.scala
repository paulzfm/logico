import Types._

/**
  * Created by paul on 29/01/2017.
  *
  * Database.
  *
  * A database containing rules and facts.
  * Internally, each rule will be indexed by an integer, which will be cited by the trace tree.
  *
  * @param rules rules and facts defined by user.
  *              NOTE that pre-defined rules will always be automatically included.
  */
class Database(val rules: List[Rule] = Nil) {
  private val predefinedRules = List(
    new Rule(Atom(Word("member-of"), List(Variable("X"),
      PList(List(Variable("X")), Any)))),
    new Rule(Atom(Word("member-of"), List(Variable("X"),
      PList(List(Any), Variable("Ys")))),
      Atom(Word("member-of"), List(Variable("X"), Variable("Ys")))),

    new Rule(Atom(Word("length-is"), List(CList(), Integer(0)))),
    new Rule(Atom(Word("length-is"), List(
      PList(List(Variable("X")), Variable("XS")), Variable("Y")
    )), Conj(List(
      Atom(Word("length-is"), List(Variable("XS"), Variable("Y1"))),
      Atom(Word("sum"), List(Variable("Y1"), Integer(1), Variable("Y")))
    ))),

    new Rule(Atom(Word("append-to"), List(CList(), Variable("Y"), Variable("Y")))),
    new Rule(Atom(Word("append-to"), List(
      PList(List(Variable("X1")), Variable("XS1")),
      Variable("Y"),
      PList(List(Variable("X1")), Variable("Z"))
    )), Atom(Word("append-to"), List(Variable("XS1"), Variable("Y"), Variable("Z")))),

    new Rule(Atom(Word("position-is"), List(Variable("X"), Variable("XS"), Variable("Y"))),
      Conj(List(
        Atom(Word("append-to"),
          List(Variable("X1"), PList(List(Variable("X")), Any), Variable("XS"))),
        Atom(Word("length-is"), List(Variable("X1"), Variable("Y1"))),
        Atom(Word("sum"), List(Variable("Y1"), Integer(1), Variable("Y")))
      )))
  )

  private val rulesWithId: List[(Int, Rule)] = predefinedRules.map((0, _)) ++
    (1 to rules.length).toList.zip(rules)

  private val hashMap: Map[Sig, List[(Int, Rule)]] = rulesWithId.groupBy(_._2.sig)

  /**
    * Query all rules which belong to the function of signature `sig`.
    *
    * @param sig function signature.
    * @return all related rules.
    */
  def get(sig: Sig): Option[List[(Int, Rule)]] = hashMap.get(sig)

  /**
    * Expand database will extra user defined rules.
    *
    * @param newRules extra user defined rules.
    * @return expanded database.
    */
  def append(newRules: List[Rule]): Database = new Database(rules ++ newRules)

  override def toString: String = rulesWithId map {
    case (id, rule) => s"($id) $rule\n"
  } mkString
}

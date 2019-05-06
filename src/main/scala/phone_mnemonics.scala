/*
  *
  *
  *   Problem
        Phone keys have mnemonics assigned to them.

        val mnemonics = Map( '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
                             '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
        Assume you are given a dictionary words as a list of words. Design a method:

        translate(phoneNumber)
        which produces all phrases of words that can serve as mnemonics for the phone number.

        Example: The phone number 7225247386 should have the mnemonic Scala is fun as one element of the set of solution phrases.
  *
  *
 */


object phone_mnemonics {
  import scala.io.Source

  val in = Source.fromURL("https://raw.githubusercontent.com/rohitvg/scala-principles-1/master/resources/data/linuxwords.txt")
  val words = in.getLines().toList filter (word => word forall (c => c.isLetter))
  /*
    *
    * scala> import scala.io.Source
      import scala.io.Source

      scala>   val in = Source.fromURL("https://raw.githubusercontent.com/rohitvg/scala-principles-1/master/resources/data/linuxwords.txt")
      in: scala.io.BufferedSource = <iterator>

      scala>   val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))
      words: List[String] = List(P, Q, S, PA, PC, Aarhus, Aaron, Ababa, aback, abaft, abandon, abandoned, abandoning, abandonment, abandons, abase, abased, abasement, abasements, abases, abash, abashed, abashes, abas
      hing, abasing, abate, abated, abatement, abatements, abater, abates, abating, Abba, abbe, abbey, abbeys, abbot, abbots, Abbott, abbreviate, abbreviated, abbreviates, abbreviating, abbreviation, abbreviations, A
      bby, abdomen, abdomens, abdominal, abduct, abducted, abduction, abductions, abductor, abductors, abducts, Abe, abed, Abel, Abelian, Abelson, Aberdeen, Abernathy, aberrant, aberration, aberrations, abet, abets,
      abetted, abetter, abetting, abeyance, abhor, abhorred, abhorrent, abhorrer, abhorring, abhors, abide, abided, abides, abiding, Abidjan, Abigail, Abilene, abilities, ab...
      *
      *
   */
  val mnemonics = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
  /*
    *
    * scala> mnemonics
      res7: scala.collection.immutable.Map[Char,String] = Map(8 -> TUV, 4 -> GHI, 9 -> WXYZ, 5 -> JKL, 6 -> MNO, 2 -> ABC, 7 -> PQRS, 3 -> DEF)
    *
   */


  // maps letters to digits; eg. '2' -> 'A', '2' -> 'B', etc.
  val charCode: Map[Char, Char] = {
    for {
      (digit, string) <- mnemonics
      letter <- string
    } yield letter -> digit
  }

  /*
    * scala> charCode
    * res6: Map[Char,Char] = Map(E -> 3, X -> 9, N -> 6, T -> 8, Y -> 9, J -> 5, U -> 8, F -> 3, A -> 2, M -> 6, I -> 4, G -> 4, V -> 8, Q -> 7, L -> 5, B -> 2, P -> 7, C -> 2, H -> 4, W -> 9, K -> 5, R -> 7, O -> 6,
    * D -> 3, Z -> 9, S -> 7)
   */


  // maps strings to letters eg. "Java" -> "5282"
  def wordCode(word: String): String = word.toUpperCase map charCode
  /*
    *
    * scala> wordCode("jaba")
      res3: String = 5222

      scala> wordCode("java")
      res4: String = 5282

      scala> wordCode("javA")
      res5: String = 5282
    *
   */
  // map from digits to words that represent them eg. "5282" -> List("Java", "Lava", "Kata", ...)
  val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue Seq()
  /*
    *
    * scala> wordsForNum
      res0: Map[String,Seq[String]] = Map(63972278 -> List(newscast), 29237638427 -> List(cybernetics), 782754448 -> List(starlight), 2559464 -> List(allying), 862532733 -> List(uncleared), 365692259 -> List(enjoyabl
      y), 868437 -> List(unties), 33767833 -> List(deportee), 742533 -> List(picked), 3364646489 -> List(femininity), 3987267346279 -> List(extraordinary), 7855397 -> List(pulleys), 67846493 -> List(optimize), 472383
      7 -> List(grafter), 386583 -> List(evolve), 78475464 -> List(Stirling), 746459 -> List(singly), 847827 -> List(vistas), 546637737 -> List(lionesses), 28754283 -> List(curlicue), 84863372658 -> List(thunderbolt)
      , 46767833 -> List(imported), 26437464 -> List(angering, cohering), 8872267 -> List(turbans), 77665377 -> List(spoolers), 46636233 -> List(homemade), 7446768759 -> Lis...

    *
    *
   */

  def encode(phoneNumber: String): Set[List[String]] = {
    if (phoneNumber.isEmpty) Set(List())
    else {
      (for {
        split <- 1 to phoneNumber.length
        word <- wordsForNum(phoneNumber take split)
        other <- encode(phoneNumber drop split)
      } yield word :: other).toSet
    }
  }

  def translate(phoneNumber: String): Set[String] = encode(phoneNumber) map(_ mkString " ")
    /*
      *
      * scala> import phone_mnemonics._
        import phone_mnemonics._
      *
      * encode("7225247386")
      * scala> encode("7225247386")
        res1: Set[List[String]] = Set(List(rack, ah, re, to), List(sack, ah, S, fun), List(PC, Claire, to), List(pack, ah, P, fun), List(PA, Al, ah, S, fun), List(sack, ah, Q, fun), List(S, Calais, fun), List(sack, ah,
         re, to), List(Scala, ire, to), List(PA, Al, air, fun), List(PC, Al, ah, Q, fun), List(PA, Al, ah, P, fun), List(P, Calais, fun), List(sack, ah, P, fun), List(PC, Al, air, fun), List(rack, ah, P, fun), List(sac
        k, air, fun), List(rack, air, fun), List(rack, ah, Q, fun), List(S, Baja, is, fun), List(rack, bird, to), List(pack, air, fun), List(PA, Blair, fun), List(PC, Al, ah, re, to), List(pack, ah, re, to), List(PC, A
        l, ah, S, fun), List(pack, bird, to), List(P, Baja, is, fun), List(PC, Al, ah, P, fun), List(S, Baja, ire, to), List(rack, ah, S, fun), List(PC, Blair, fun), List(pack...

      *
      * res2: Set[String] = Set(sack air fun, pack ah re to, sack ah P fun, P Calais fun, sack ah Q fun, pack bird to, PC Al ah P fun, Scala ire to, Q Calais fun, PC Al air fun, Scala is fun, PA Al ah re to, PC Claire
        to, rack ah re to, pack ah S fun, rack ah S fun, rack ah Q fun, Q Baja ire to, pack ah P fun, P Baja ire to, PA Al ah Q fun, S Baja is fun, P Baja is fun, PC Blair fun, rack ah P fun, pack ah Q fun, pack air fu
        n, S Baja ire to, PA Al ah P fun, PA Al air fun, PA Al ah S fun, PA Al bird to, sack bird to, rack bird to, PA Claire to, Q Baja is fun, PC Al ah Q fun, sack ah re to, rack air fun, S Calais fun, PC Al bird to,
         PC Al ah re to, PA Blair fun, PC Al ah S fun, sack ah S fun)

      *
      * scala> translate("7225247386").filter(x=>x=="Scala is fun")
        res3: scala.collection.immutable.Set[String] = Set(Scala is fun)
      *
     */


}

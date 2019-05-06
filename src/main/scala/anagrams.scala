import scala.io.Source

object Anagrams {


  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
    * how often the character appears.
    * This list is sorted alphabetically w.r.t. to the character in each pair.
    * All characters in the occurrence list are lowercase.
    *
    * Any list of pairs of lowercase characters and their frequency which is not sorted
    * is **not** an occurrence list.
    *
    * Note: If the frequency of some character is zero, then that character should not be
    * in the list.
    */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
    * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
    */
  val in = Source.fromURL("https://raw.githubusercontent.com/rohitvg/scala-principles-1/master/resources/data/linuxwords.txt")
  val dictionary = in.getLines().toList filter (word => word forall (c => c.isLetter))

//  val dictionary: List[Word] = loadDictionary

  /*
    *
    * scala> dictionary
      res54: List[forcomp.Anagrams.Word] = List(Aarhus, Aaron, Ababa, aback, abaft, abandon, abandoned, abandoning, abandonment, abandons, abase, abased, abasement, abasements, abases, abash, abashed, abashes, abashi
      ng, abasing, abate, abated, abatement, abatements, abater, abates, abating, Abba, abbe, abbey, abbeys, abbot, abbots, Abbott, abbreviate, abbreviated, abbreviates, abbreviating, abbreviation, abbreviations, Abb
      y, abdomen, abdomens, abdominal, abduct, abducted, abduction, abductions, abductor, abductors, abducts, Abe, abed, Abel, Abelian, Abelson, Aberdeen, Abernathy, aberrant, aberration, aberrations, abet, abets, ab
      etted, abetter, abetting, abeyance, abhor, abhorred, abhorrent, abhorrer, abhorring, abhors, abide, abided, abides, abiding, Abidjan, Abigail, Abilene, abilities, abil...

    *
   */

  /** Converts the word into its character occurrence list.
    *
    * Note: the uppercase and lowercase version of the character are treated as the
    * same character, and are represented as a lowercase character in the occurrence list.
    *
    * Note: you must use `groupBy` to implement this method!
    */
  def wordOccurrences(w: Word): Occurrences = {
    val list = w.toLowerCase.filter(x => x.isLetter).groupBy(elem => (elem))
    ((for {
      (char, char_list) <- list
    } yield (char, char_list.length)).toList).sorted
  }

  def wordOccurrences2(w: Word): Occurrences =
    w.toLowerCase.filter(x => x.isLetter).groupBy(elem => (elem)).map(x => (x._1, x._2.length)).toList.sorted //.sortBy(x=> -x._2)
  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString(""))

  /*
    *
    *
    * scala> wordOccurrences2("eat")
      res51: forcomp.Anagrams.Occurrences = List((e,1), (t,1), (a,1))

    *
    * scala> sentenceOccurrences(List("eat","Hello"))
      res53: forcomp.Anagrams.Occurrences = List((a,1), (e,2), (h,1), (l,2), (o,1), (t,1))

    *
   */
  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
    * the words that have that occurrence count.
    * This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
    *
    * For example, the word "eat" has the following character occurrence list:
    *
    * `List(('a', 1), ('e', 1), ('t', 1))`
    *
    * Incidentally, so do the words "ate" and "tea".
    *
    * This means that the `dictionaryByOccurrences` map will contain an entry:
    *
    * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
    *
    */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    dictionary.groupBy(x => wordOccurrences(x))
  }
  /*
    *
    * scala> dictionaryByOccurrences
      res49: Map[forcomp.Anagrams.Occurrences,List[forcomp.Anagrams.Word]] = Map(List((e,1), (i,1), (l,1), (r,1), (t,2)) -> List(litter), List((a,1), (d,1), (e,1), (g,2), (l,1), (r,1)) -> List(gargled), List((a,1), (
      e,1), (h,1), (i,1), (k,1), (n,1), (s,3)) -> List(shakiness), List((e,2), (g,1), (n,1)) -> List(gene), List((a,2), (n,1), (t,1), (y,1)) -> List(Tanya), List((a,1), (d,1), (e,2), (h,1), (m,1), (n,2), (o,1), (s,3)
      ) -> List(handsomeness), List((a,2), (c,1), (e,2), (k,1), (l,1), (m,1), (p,1), (r,1), (t,1)) -> List(marketplace), List((a,1), (i,1), (l,2), (s,1), (v,1)) -> List(villas), List((d,2), (e,1), (h,2), (n,1), (r,1)
      , (t,1), (u,1)) -> List(hundredth), List((a,3), (b,1), (c,1), (h,1), (i,2), (l,1), (o,1), (p,2), (r,1), (t,1), (y,1)) -> List(approachability), List((d,1), (e,2), (l,1...

    *
   */

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    dictionaryByOccurrences(wordOccurrences(word))
  }

  /*
    *
    * scala> dictionaryByOccurrences(wordOccurrences("eat"))
      res48: List[forcomp.Anagrams.Word] = List(ate, eat, tea)
    *
   */

  /** Returns the list of all subsets of the occurrence list.
    * This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
    * is a subset of `List(('k', 1), ('o', 1))`.
    * It also include the empty subset `List()`.
    *
    * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
    *
    * List(
    * List(),
    * List(('a', 1)),
    * List(('a', 2)),
    * List(('b', 1)),
    * List(('a', 1), ('b', 1)),
    * List(('a', 2), ('b', 1)),
    * List(('b', 2)),
    * List(('a', 1), ('b', 2)),
    * List(('a', 2), ('b', 2))
    * )
    *
    * Note that the order of the occurrence list subsets does not matter -- the subsets
    * in the example above could have been displayed in some other order.
    */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    val startlist = occurrences.map(a => (0 to a._2).map(y => (a._1, y)).toList)
    startlist.foldRight(List[Occurrences](List()))((x,y) => combinetwolists(x,y))
  }
  def combinetwolists(xs:List[(Char,Int)],ys:List[Occurrences]):List[Occurrences] = {
    if(ys.isEmpty) {
      if(xs.isEmpty) List(xs)
      else
        for{
          x <- xs
        } yield List(x).filter(elem=>elem._2 != 0)
    }
    else
      for{
        x <- xs
        y <- ys
      } yield (x :: y).filter(elem=>elem._2 != 0)
  }



  /** Subtracts occurrence list `y` from occurrence list `x`.
    *
    * The precondition is that the occurrence list `y` is a subset of
    * the occurrence list `x` -- any character appearing in `y` must
    * appear in `x`, and its frequency in `y` must be smaller or equal
    * than its frequency in `x`.
    *
    * Note: the resulting value is an occurrence - meaning it is sorted
    * and has no zero-entries.
    */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val xmap = x.toMap withDefaultValue 0
    val ymap = y.toMap withDefaultValue 0
    (for {
      key <- xmap.keys ++ ymap.keys
    } yield (key, xmap(key) - ymap(key))).filter(pair => pair._2 > 0).toList.sortBy(x=>x._1)
  }


  /** Returns a list of all anagram sentences of the given sentence.
    *
    * An anagram of a sentence is formed by taking the occurrences of all the characters of
    * all the words in the sentence, and producing all possible combinations of words with those characters,
    * such that the words have to be from the dictionary.
    *
    * The number of words in the sentence and its anagrams does not have to correspond.
    * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
    *
    * Also, two sentences with the same words but in a different order are considered two different anagrams.
    * For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
    * `List("I", "love", "you")`.
    *
    * Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
    *
    * List(
    * List(en, as, my),
    * List(en, my, as),
    * List(man, yes),
    * List(men, say),
    * List(as, en, my),
    * List(as, my, en),
    * List(sane, my),
    * List(Sean, my),
    * List(my, en, as),
    * List(my, as, en),
    * List(my, sane),
    * List(my, Sean),
    * List(say, men),
    * List(yes, man)
    * )
    *
    * The different sentences do not have to be output in the order shown above - any order is fine as long as
    * all the anagrams are there. Every returned word has to exist in the dictionary.
    *
    * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
    * so it has to be returned in this list.
    *
    * Note: There is only one anagram of an empty sentence.
    */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def recurse(some_occurrence: Occurrences): List[Sentence] = {
      if (some_occurrence.isEmpty) List(Nil)
      else
        for {
          split <- combinations(some_occurrence) filter dictionaryByOccurrences.contains
          word_from_words_on_split <- dictionaryByOccurrences(split)
          other <- recurse(subtract(some_occurrence, split))
        } yield word_from_words_on_split :: other
    }
    recurse(sentenceOccurrences(sentence))

  }
}

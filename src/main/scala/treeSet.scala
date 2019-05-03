/*   We cannot instantiate an abstract class. We can just define the function types without having a body in an abstract class.
 *   We will create a tree to store the numbers in the set. The tree consists of two possible entities.
 *   an empty set
 *   a full set
 *
 *   Note that in these structures, we don't change any of the data structures, we only create new ones.
 *   Such data structures are called persistent data structures. If you didn't get the below, reread the code
 *   for include method in the fullSet below
 *
 *   Persistent data structures are super important in scaling up these functional programming classes into collections
 *
 *   scala> val a = emptySet
 *   a: emptySet = .
 *
 *   scala> a.incl(4).incl(2).incl(1)
 *   res0: treeSet = {{{.|1|.}|2|.}|4|.}
 *
 *   We could have used class emptySet below (in this case, to initialize, please use "val a = new emptySet"
 *   We note that such an implementation is overkill because each time a new empty part of the tree is created,
 *   we only use the same thing
 *
 *   Therefore we might replace the class emptySet with an object emptySet
 *   class emptySet extends treeSet {
 *
 *   }
 *
 *

*/

abstract class treeSet {
  //  Include the element in the set
  def incl(x: Int): treeSet

  //  Does the intset contain the Int x?
  def contains(x: Int): Boolean

  def union(otherSet: treeSet): treeSet
}

object emptySet extends treeSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): treeSet = new fullSet(x, emptySet, emptySet)

  override def union(otherSet: treeSet): treeSet = {
    otherSet
  }

  override def toString: String = "."
}

class fullSet(x: Int, left_subtree: treeSet, right_subtree: treeSet) extends treeSet {
  def nodeval: Int = x

  def lst: treeSet = left_subtree

  def rst: treeSet = right_subtree

  def contains(x: Int): Boolean = {
    if (x == nodeval)
      true
    else if (x < nodeval)
      lst.contains(x)
    else rst.contains(x)
  }

  def incl(x: Int): treeSet = {
    if (x < nodeval)
      new fullSet(nodeval, lst.incl(x), rst)
    else if (x > nodeval)
      new fullSet(nodeval, lst, rst.incl(x))
    else this
  }

  override def union(otherSet: treeSet): treeSet = {
    if (otherSet == emptySet) this else {
      otherSet.union(lst).incl(nodeval).union(rst)
    }
  }

  override def toString: String = "{" + lst.toString + "|" + nodeval + "|" + rst.toString + "}"

}
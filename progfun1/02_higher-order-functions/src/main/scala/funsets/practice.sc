import funsets.FunSets.{Set}

object practice {
  /**
    * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.
    */
  def singletonSet(elem: Int): Set = Set(elem)


  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(s: Set, t: Set): Set = (item: Int) => s(item) | t(item)

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(s: Set, t: Set): Set = (item: Int) => s(item) & t(item)

  /**
    * Returns the difference of the two given lets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = (item: Int) => s(item) & !t(item)

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter(s: Set, p: Int => Boolean): Set = (item: Int) => s(item) & p(item)

  union(Set(1), Set(2))(2)
  union(Set(1), Set(2))(3)
  intersect(Set(1,2), Set(2))(1)
  intersect(Set(1,2), Set(2))(2)
  diff(Set(1,2), Set(2))(1)
  diff(Set(1,2), Set(2))(2)
  filter(Set(1,2,3), x => x == 1)(2)

  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  val bound = 1000
  def forall(s: Set, p: Int => Boolean): Boolean = (for(i <- -bound to bound if contains(s,i)) yield p(i)).reduce(_&&_)
  def exists(s: Set, p: Int => Boolean): Boolean = (for(i <- -bound to bound if contains(s,i)) yield p(i)).reduce(_||_)

  forall(Set(-1,3,9), (x: Int) => (x % 3) == 0)
  exists(Set(-1,1,10), (x: Int) => (x % 3) == 0)

  def map(s: Set, f: Int => Int): Set = (a: Int) => (for(i <- -bound to bound if contains(s,i)) yield f(i)) contains a
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }
  toString(map(Set(1,2,3), (x) => x + 1))
}

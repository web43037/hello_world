package objsets

import java.util.NoSuchElementException

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    text + " (" + user + ") " + "[" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's tex
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  val isEmpty: Boolean

  /**
   * This method takxes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet = {
    println("abstract.in filter")
    val acc = new Empty
    filterAcc(p, acc)
  }

  /**
   * This is a helper method for `filter` that propagates the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = ???

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet = ???

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted(tw: Tweet): Tweet = ???

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList = ???

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def chgSet(): Set[Tweet] = {
    var res = Set[Tweet]()
    foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  // def traverse(p: Tweet => Unit): Unit

}

class Empty extends TweetSet {

  override def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    println("Empty.filterAcc")
    acc
  }

  override val isEmpty = true

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  override def union(other: TweetSet): TweetSet = {
    //println("new Empty.union: " + other)
    other
  }

  override def mostRetweeted(tw: Tweet): Tweet = throw new java.util.NoSuchElementException("Empty TweetSet has no tweets to compare.")

}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  override val isEmpty = false

  //[2016-08-12 Fri] my work here
  // override def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
  //   if (true) acc WHAT IS THE BASE CASE?
  //   filterAcc(p, left)
  //   if (p(elem) == true) acc.incl(elem)
  //   filterAcc(p, right)

  // [2016-08-12 Fri] "borrowed code" that actually works
  override def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    val l = left.filterAcc(p, acc)
    val r = right.filterAcc(p, acc)
    val lr = l union r
    if (p(elem)) lr.incl(elem) else lr
  }

  override def union(other: TweetSet): TweetSet = {
    (left union (right union other)) incl elem
  }

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  override def mostRetweeted(tw: Tweet): Tweet = {
    val high = if (tw.retweets < elem.retweets) elem else tw
    if (left.isEmpty & right.isEmpty) high
    else if (left.isEmpty) right.mostRetweeted(high)
    else if (right.isEmpty) left.mostRetweeted(high)
    else { // none are empty
      left.mostRetweeted(high)
      right.mostRetweeted(high)
    }
  }

  // override def descendingByRetweet: TweetList = {
//     var tail: TweetList // 
// //     try {
// //       while (true) {
//         val ls = new Cons(mostRetweeted(elem), tail) // 
//         val set = remove(tw)
//         tail = ls}
//     } catch {
//         case nse: java.util.NoSuchElementException => list
//     }
//     }

  def toList(tw: Tweet): TweetList= {
    val seed = elem
    var lll = new Cons(seed, Nil)
    //loop
    val tweet_remove = mostRetweeted(seed)
    val shrinking_set = remove(tweet_remove)
    lll = new Cons(tweet_remove, old-Cons
      // end loop
    }

  }

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = ???
  //lazy val appleTweets: TweetSet = ???

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = ???
}


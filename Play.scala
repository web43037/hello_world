import objsets._
import java.util.NoSuchElementException

object Play extends App {
  println("In Play main()")
  val set1 = new Empty
  val b = new Tweet("b", "b body", 20)
  val c = new Tweet("c", "c body", 7)
  val d = new Tweet("d", "d body", 9)
  val a = new Tweet("a", "a body", 2)
  val e = new Tweet("e", "e body", 35)
  val tw1 = new Tweet("user", "Kindle Paperwhite Review: Forget Everything Else,", 33)
  val tw2 = new Tweet("Craig", "O be wise what more can I say.", 16)

  val set2 = set1.incl(a)
  val set3 = set2.incl(b)
  val set4c = set3.incl(c)
  val set4d = set4c.incl(d)
  val set5 = set4d.incl(e)
  val set6 = set5.incl(tw1)
  val set7 = set6.incl(tw1)
  val set8 = set7.incl(tw2)

  println(tw1) //> User: user
  //TweetData.gizmodo
  //  val gizmodoTweets = TweetReader.ParseTweets.getTweetData("gizmodo", TweetData.gizmodo)

  // gizmodoTweets.foreach { t: Tweet => println(t) } //> User: gizmodo
  //val set_tweets = TweetReader.toTweetSet(gizmodoTweets)
  println("size: " + set8.size(set8))

  // def traverse(tws: TweetSet): Unit = {
  //   if (tws==null) return
  //     traverse(tws.left)
  //   }
  // set5.traverse(p => println(p))

  val acc = new Empty
  // set5.filterAcc(t => true, acc) 
  println("--------------------------------")
  set8.foreach(println _)
  println("--------------------------------")
  println("Ans: " + set8.mostRetweeted(a))
  //try {
  //   val tw = set8.mostRetweeted(a)
  //   //println("Ans: " + tw)
  // } catch {
  //   case ex: java.util.NoSuchElementException => println("Exception: " + ex)
  // }

  val ntw1 = set8.mostRetweeted(a)
  val tws1 = set8.remove(ntw1)
  val list1 = new Cons(ntw1, Nil)

  val ntw2 = tws1.mostRetweeted(a)
  val tsw2 = tws1.remove(ntw2)
  val list2 = new Cons(ntw2, list1)

  val ntw3 = tsw2.mostRetweeted(a)
  val tsw3 = tsw2.remove(ntw3)
  val list3 = new Cons(ntw3, list2)

  println("--------------------------------")
  list3.foreach(println(_))



}

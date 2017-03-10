package rvolkert.fp.stream

import rvolkert.fp.stream.Stream._
import rvolkert.fp.stream.StreamRecursion._
import org.specs2.mutable.Specification

object StreamSpec extends Specification {
  "toList" in {
    Cons(() => 1, () => Cons(() => 2, () => Cons(() => 3, () => Empty))).toList === List(1,2,3)
    cons(1, cons(2, cons(3, Empty))).toList === List(1,2,3)
  }
  "take" in {
    cons(1, cons(2, cons(3, Empty))).take(2).toList === List(1,2)
    cons(1, cons(2, cons(3, Empty))).take(0).toList === List()
    cons(1, cons(2, cons(3, Empty))).take(4).toList === List(1,2,3)
    Empty.take(1).toList === List()

    // cons(1, cons(2, cons(3, Empty))).take(2) === cons(1, cons(2, Empty))
    // cons(1, cons(2, cons(3, Empty))).take(0) === Empty
    // cons(1, cons(2, cons(3, Empty))).take(4) === cons(1, cons(2, cons(3, Empty)))
    // Empty.take(1) === Empty
  }
  "drop" in {
    cons(1, cons(2, cons(3, Empty))).drop(2).toList === List(3)
    cons(1, cons(2, cons(3, Empty))).drop(0).toList === List(1,2,3)
    cons(1, cons(2, cons(3, Empty))).drop(3).toList === List()
    cons(1, cons(2, cons(3, Empty))).drop(4).toList === List()
    Empty.drop(1).toList === List()

    // cons(1, cons(2, cons(3, Empty))).drop(2) === cons(3, Empty)
    // cons(1, cons(2, cons(3, Empty))).drop(0) === cons(1, cons(2, cons(3, Empty)))
    // cons(1, cons(2, cons(3, Empty))).drop(3) === Empty
    // cons(1, cons(2, cons(3, Empty))).drop(4) === Empty
    // Empty.drop(1) === Empty
  }

  "takeWhile" in {
    cons(1, cons(2, cons(3, Empty))).takeWhile(_ % 2 == 1).toList === List(1)
    Empty.takeWhile((i: Int) => true) === Empty
  }

  "forAll" in {
    cons(1, cons(2, cons(3, Empty))).forAll(_ + 1 > 1) must beTrue
    cons(1, cons(2, cons(3, Empty))).forAll(_ + 1 > 2) must beFalse
    Empty.forAll((i: Int) => false) must beTrue
  }

  "takeWhile2" in {
    cons(1, cons(2, cons(3, Empty))).takeWhile2(_ % 2 == 1).toList === List(1)
    Empty.takeWhile2((i: Int) => true) === Empty
  }

  "headOption" in {
    cons(1, cons(2, cons(3, Empty))).headOption === Some(1)
    (Empty: Stream[String]).headOption === None
  }

  "map" in {
    cons(1, cons(2, cons(3, Empty))).map(_ * 2).toList === List(2,4,6)
    cons(1, cons(2, cons(3, Empty))).map(_.toString).toList === List("1", "2", "3")
    (Empty: Stream[String]).map(_.toString) === Empty
  }

  "filter" in {
    cons(1, cons(2, cons(3, Empty))).filter(_ % 2 == 1).toList === List(1,3)
    (Empty: Stream[Int]).filter(_ % 2 == 1) === Empty
    cons(1, cons(2, cons(3, Empty))).filter(_ + 1 > 2).toList === List(2,3)
  }

  "append" in {
    cons(1, cons(2, cons(3, Empty))).append(cons(4, cons(5, cons(6, Empty)))).toList === List(1,2,3,4,5,6)
    (Empty: Stream[Int]).append(cons(1, cons(2, Empty))).toList === List(1,2)
    cons(1, cons(5, Empty)).append(Empty).toList === List(1,5)
  }

  "flatMap" in {
    cons(1, cons(2, cons(3, Empty))).flatMap(a => cons(a, cons(a * 2, Empty))).toList === List(1,2,2,4,3,6)
    cons(1, cons(2, cons(3, Empty))).flatMap(a => cons(a.toString, Empty)).toList === List("1", "2", "3")
    (Empty: Stream[String]).flatMap(a => cons(a.toString, Empty)) === Empty
  }

  "constant" in {
    constant(1).take(2).toList === List(1,1)
    constant("a").take(0).toList === List()
    constant('b').take(4).toList === List('b','b','b','b')

    constant2(1).take(2).toList === List(1,1)
    constant2("a").take(0).toList === List()
    constant2('b').take(4).toList === List('b','b','b','b')
  }

  "from" in {
    from(1).take(4).toList === List(1,2,3,4)
    from(-1).take(3).toList === List(-1,0,1)
    from(0).take(2).toList === List(0,1)

    from2(1).take(4).toList === List(1,2,3,4)
    from2(-1).take(3).toList === List(-1,0,1)
    from2(0).take(2).toList === List(0,1)
  }

  "fibs" in {
    fibs().take(7).toList === List(0, 1, 1, 2, 3, 5, 8)

    fibs2().take(7).toList === List(0, 1, 1, 2, 3, 5, 8)
  }

  "unfold" in {
    unfold(1){ x => Some(x, x + 1) }.take(4).toList === List(1,2,3,4)
  }

  "map with unfold" in {
    mapU(cons(1, cons(2, cons(3, Empty))))(_ * 2).toList === List(2,4,6)
    mapU(cons(1, cons(2, cons(3, Empty))))(_.toString).toList === List("1", "2", "3")
    mapU((Empty: Stream[String]))(_.toString) === Empty
  }

  "take with unfold" in {
    takeU(cons(1, cons(2, cons(3, Empty))))(2).toList === List(1,2)
    takeU(cons(1, cons(2, cons(3, Empty))))(0).toList === List()
    takeU(cons(1, cons(2, cons(3, Empty))))(4).toList === List(1,2,3)
    takeU(Empty)(1).toList === List()
  }

  "takeWhileU" in {
    takeWhileU(cons(1, cons(2, cons(3, Empty))))(_ % 2 == 1).toList === List(1)
    takeWhileU(Empty)((i: Int) => true) === Empty
  }

  "zipWith" in {
    val s = cons(1, cons(2, cons(3, Empty)))
    zipWith(s, s)(_ + _).toList === List(2,4,6)
    zipWith(s, cons(2, cons(3, Empty)))(_ * _).toList === List(2,6)
    zipWith(cons(2, cons(3, Empty)), s)(_ * _).toList === List(2,6)
    zipWith(Empty: Stream[Int], cons(1, Empty))(_ + _) === Empty
  }

  "zipAll" in {
    val i = cons(1, cons(2, cons(3, Empty)))
    val s = cons("1", cons("2", cons("3", Empty)))
    zipAll(i, s).toList === List((Some(1), Some("1")), (Some(2), Some("2")), (Some(3), Some("3")))
    zipAll(cons("0", s), cons(0, cons(1, Empty))).toList === List((Some("0"), Some(0)), (Some("1"), Some(1)), (Some("2"), None), (Some("3"), None))
    zipAll(cons("0", cons("1", Empty)), cons(0, i)).toList === List((Some("0"), Some(0)), (Some("1"), Some(1)), (None, Some(2)), (None, Some(3)))
    zipAll(Empty: Stream[Int], Empty: Stream[String]) === Empty
  }

  "tails" in {
    val i = cons(1, cons(2, cons(3, Empty)))
    tails(i).take(5).toList.map(_.toList) === List(List(1,2,3), List(2,3), List(3), List())
  }
}


















//hi

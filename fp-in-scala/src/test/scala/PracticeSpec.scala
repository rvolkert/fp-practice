package rvolkert.fp

import rvolkert.fp.List._
import rvolkert.fp.Tree._
import org.specs2.mutable.Specification
import Practice._
import Option._
import Either._

object PracticeSpec extends Specification {

  "fib" should {
    "return the next Fibonacci number (where 0 is first, 1 is second, ...)" in {
      fib(1) === 0
      fib(2) === 1
      fib(3) === 1
      fib(4) === 2
      fib(5) === 3
      fib(6) === 5
    }
  }

  "isSorted" should {
    "work for an array of length 0" in {
      isSorted[Int](Array(), (f: Int, g: Int) => f <= g) must beTrue
    }
    "work for an array of length 1" in {
      isSorted[Int](Array(1), (f: Int, g: Int) => f <= g) must beTrue
    }
    "work for an array of length 2" in {
      isSorted[Int](Array(1,2), (f: Int, g: Int) => f <= g) must beTrue
      isSorted[Int](Array(3,2), (f: Int, g: Int) => f <= g) must beFalse
    }
    "work for an array of length 3" in {
      isSorted[Int](Array(1,2,3), (f: Int, g: Int) => f <= g) must beTrue
      isSorted[Int](Array(3,2,4), (f: Int, g: Int) => f <= g) must beFalse
    }
  }

  "tail" should {
    "work for an empty list" in {
      tail(Nil) === Nil
    }
    "work for a list of length 1" in {
      tail(List(1)) === Nil
    }
    "work for a list of length 2" in {
      tail(List(1,2)) === List(2)
    }
    "work for a list of length 3" in {
      tail(List("a", "b", "c")) === List("b", "c")
    }
  }

  "setHead" should {
    "work for an empty list" in {
      setHead(Nil, 1) === Nil
    }
    "work for a list of length 2" in {
      setHead(List(1,2), 3) === List(3, 2)
    }
    "work for a list of length 3" in {
      setHead(List("a", "b", "c"), "d") === List("d", "b", "c")
    }
  }

  "drop" should {
    "work for an empty list" in {
      drop(Nil, 1) === Nil
    }
    "work for a number greater than the length of the list" in {
      drop(List(1,2), 3) === Nil
    }
    "remove the first element" in {
      drop(List("a", "b", "c"), 1) === List("b", "c")
    }
    "remove the first two elements" in {
      drop(List("a", "b", "c"), 2) === List("c")
    }
  }

  "dropWhile" should {
    "work for an empty list" in {
      dropWhile[Int](Nil, {_ == 1}) === Nil
    }
    "drop all even numbers until there's an odd number" in {
      dropWhile(List(2,4,6,3,4,8), {x: Int => x % 2 == 0}) === List(3,4,8)
    }
    "drop all strings that match the predicate" in {
      dropWhile(List("a", "b", "c"), {_: String => true}) === Nil
    }
  }

  "init" should {
    "work for an empty list" in {
      init(Nil) === Nil
    }
    "work for a list of length 1" in {
      init(List(true)) === Nil
    }
    "work for a list of length 2" in {
      init(List(1,2)) === List(1)
    }
    "work for a list of length 3" in {
      init(List("a", "b", "c")) === List("a", "b")
    }
  }

  "foldRight" should {
    "reconstruct the list" in {
      foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) === List(1,2,3)
    }
  }

  "lengthOfList" should {
    "find the lengths of lists" in {
      lengthOfList(Nil) === 0
      lengthOfList(List(1)) === 1
      lengthOfList(List("a", "b")) === 2
    }
  }

  "foldLeft1" should {
    "reverse the list" in {
      foldLeft1(List(1,2,3), Nil:List[Int])((b, a) => Cons(a, b)) === List(3,2,1)
    }
    "add the numbers" in {
      foldLeft1(List(1,2,3), 0)(_ + _) === 6
    }
  }

  "sumL" should {
    "sum any kind of numbers with +" in {
      sumL(Nil) === 0
      sumL(List(1.1, 2.2, 3.3)) === 6.6
      sumL(List(2, 4.2, 6L)) === 12.2
      sumL(List(0, 1, 2)) === 3
    }
  }

  "productL" should {
    "take the product of any kind of numbers with *" in {
      productL(List(1.5, 2.0)) === 3.0
      productL(List(2, 4.25, 2L)) === 17.0
      productL(List(0, 1, 2)) === 0
      productL(Nil) === 1
    }
  }

  "lengthL" should {
    "find the length of a list" in {
      lengthL(List(1.1, 2.2, 3.3)) === 3
      lengthL(List("a", "b")) === 2
      lengthL(Nil) === 0
    }
  }

  "appendList" should {
    "add a list to the end of a list (appendListR)" in {
      appendListR(List(1,2,3), List(4,5)) === List(1,2,3,4,5)
    }
    "add a list to the end of a list (appendListL)" in {
      appendListL(List(1,2,3), List(4,5)) === List(1,2,3,4,5)
    }
  }

  "reverse" should {
    "reverse any list (using foldLeft1)" in {
      reverseL(List(0, 1, 2)) === List(2, 1, 0)
      reverseL(List(1)) === List(1)
      reverseL(Nil) === Nil
    }
    "reverse any list (using foldRight)" in {
      reverseR(List(0, 1, 2)) === List(2, 1, 0)
      reverseR(List(1)) === List(1)
      reverseR(Nil) === Nil
    }
  }

  "the folds in terms of each other" should {
    "be able to do the same things as foldLeft1" in {
      foldLeftR(List(1,2,3), Nil:List[Int])((b, a) => Cons(a, b)) === List(3,2,1)
      foldLeftR(List(1,2,3), 0)(_ + _) === 6
    }
    "be able to do the same things as foldRight" in {
      foldRightL(List(1,2,3), Nil:List[Int])(Cons(_,_)) === List(1,2,3)
    }
  }

  "append" should {
    "append an element to the end of a list (using foldRight)" in {
      append(Nil, 1) === List(1)
      append(List(1, 2, 3), 4) === List(1, 2, 3, 4)
    }
    "append an element to the end of a list (using foldLeft1)" in {
      appendL(Nil, 1) === List(1)
      appendL(List(1, 2, 3), 4) === List(1, 2, 3, 4)
    }
  }

  "concatList" should {
    "have a proper helper function" in {
      prependListLeftToRight(List(3,4,5), List(2,1)) === List(1,2,3,4,5)
    }
    "combine the lists of lists into one list in linear time" in {
      concatList(List(List(1,2), List(3), Nil, List(4,5,6))) === List(1,2,3,4,5,6)
    }
  }

  "3.16 - 3.24" should {
    "add one to each int in the list" in {
      addOne(List(1,2,3)) === List(2,3,4)
      addOne(List(-1)) === List(0)
    }
    "make a list of doubles into a list of strings" in {
      doublesToStrings(List(1.1, 2, 3)) === List("1.1", "2.0", "3.0")
    }
    "map a function to each list element" in {
      map1(List(1,2,3))(_ + 1) === List(2,3,4)
      addOne2(List(0,1)) === List(1,2)
      doublesToStrings2(List(5)) === List("5.0")
    }
    "filter according to a predicate" in {
      filter1(List(1,2,3,4))(_ != 3) === List(1,2,4)
      removeOdds(List(1,2,3,4,5)) === List(2,4)
    }
    "flatMap over a list with a function that returns another list" in {
      flatMap1(List(0, 1, 2))(i => List(i, i+1, i+2)) === List(0,1,2, 1,2,3, 2,3,4)
    }
    "filter using flatMap" in {
      filter2(List("abc", "b", "a", "hello"))(a => a.contains('a')) === List("abc", "a")
    }
    "add the corresponding ints in a list (and drop the extra ints in the larger list)" in {
      addIntLists(List(0,1,2), List(1,2,3,4)) === List(1,3,5)
      addIntLists(List(0,1,2,4), List(1,2,3)) === List(1,3,5)
    }
    "zip two lists" in {
      zipWith(List(0,1,2), List(1,2,3,4))(_ + _) === List(1,3,5)
      zipWith(List("h","h"), List("i", "ello"))(_ + _) === List("hi", "hello")
      zipWith(List(0,1,2,4), List(1,2,3))(_ * _) === List(0,2,6)
    }
  }
  "3.25 - 3.29" should {
    "find the size (count the leaves and branches) of a tree" in {
      size1(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) === 5
      size2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) === 5
    }
    "find the maximum value of an int tree" in {
      maximum(Branch(Leaf(1), Branch(Leaf(4), Leaf(3)))) === 4
      maximum(Branch(Leaf(-1), Branch(Leaf(-4), Leaf(0)))) === 0
      maximum2(Branch(Leaf(1), Branch(Leaf(4), Leaf(3)))) === 4
      maximum3(Branch(Leaf(1), Branch(Leaf(4), Leaf(3)))) === 4
    }
    "find the depth (max length path) of a tree" in {
      depth(Branch(Leaf(1), Branch(Leaf(4), Leaf(3)))) === 2
    }
    "map over the values in a tree" in {
      mapTree(Branch(Leaf(1), Branch(Leaf(4), Leaf(3))))(_ * 2) === Branch(Leaf(2), Branch(Leaf(8), Leaf(6)))
    }
    "fold the values of a tree" in {
      foldTree(Branch(Leaf(1), Branch(Leaf(4), Leaf(3))), Int.MinValue)(_ max _) === 4
    }
  }

  "Options" should {
    "map" in {
      Some(1).map(_ + 1) === Some(2)
    }
    "flatMap" in {
      Some(1).flatMap(Some(_)) === Some(1)
      None.flatMap(Some(_)) === None
    }
    "getOrElse" in {
      Some(1).getOrElse(5) === 1
      None.getOrElse(5) === 5
    }
    "orElse" in {
      Some(1).orElse(Some(2)) === Some(1)
      None.orElse(Some(2)) === Some(2)
    }
    "filter" in {
      Some(1).filter(_ == 1) === Some(1)
      Some(2).filter(_ == 1) === None
    }
    "work for mean" in {
      mean(Seq(1,2,4,5)) === Some(3)
    }
    "work for variance" in {
      variance(Seq(1,2,4,5)) === Some(2.5)
    }
    "map2" in {
      map2(Some("a"), Some("b"))(_ + _) === Some("ab")
      map2(Some("a"), None)(_ + _) === None
      map2(None, Some("b"))((s1: String, s2: String) => s1 + s2) === None
    }
    "sequence" in {
      Option.sequence(List(Some(1), Some(2), Some(3))) === Some(List(1,2,3))
      Option.sequence(List(Some(1), None, Some(3))) === None
    }
    "traverse" in {
      Option.traverse(List(1,2,3))(Some(_)) === Some(List(1,2,3))
      def evenToNone(x: Int): Option[Int] = if (x % 2 == 0) None else Some(x)
      Option.traverse(List(1,2,3))(evenToNone) === None
    }
    "sequence2" in {
      Option.sequence2(List(Some(1), Some(2), Some(3))) === Some(List(1,2,3))
      Option.sequence2(List(Some(1), None, Some(3))) === None
    }
  }
  "Eithers" should {
    "map" in {
      Right(1).map(_ + 5) === Right(6)
      (Left("wrong"): Either[String, Int]).map(_ + 5) === Left("wrong")
    }
    "flatMap" in {
      Right(1).flatMap((i: Int) => Right(i + 5)) === Right(6)
      (Left("wrong"): Either[String, Int]).flatMap((i: Int) => Right(i + 5)) === Left("wrong")
    }
    "getOrElse" in {
      Right(1).getOrElse(2) === 1
      Left("wrong").getOrElse(2) === 2
    }
    "orElse" in {
      Right(1).orElse(Right(2)) === Right(1)
      Left("wrong").orElse(Right(2)) === Right(2)
    }
    "map2" in {
      Right(5).map2(Right(2))(_ * _) === Right(10)
      (Left("wrong"): Either[String, Int]).map2(Right(2))(_ * _) === Left("wrong")
      Right(3).map2(Left("wrong"))((x: Int, y: Int) => x * y) === Left("wrong")
    }
    "sequence" in {
      Either.sequence(List(Right(1), Right(2), Right(3))) === Right(List(1,2,3))
      Either.sequence(List(Right(1), Left("hi"), Left("bye"), Right(4))) === Left("hi")
    }
    "traverse" in {
      Either.traverse(List(1,2,3))(Right(_)) === Right(List(1,2,3))
      def oddToLeft(x: Int): Either[String, Int] = if (x % 2 != 0) Left(s"$x is odd") else Right(x)
      Either.traverse(List(1,2,3))(oddToLeft) === Left("1 is odd")
    }
    "sequence" in {
      Either.sequence2(List(Right(1), Right(2), Right(3))) === Right(List(1,2,3))
      Either.sequence2(List(Right(1), Left("hi"), Left("bye"), Right(4))) === Left("hi")
    }
  }
}

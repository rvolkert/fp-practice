package rvolkert.bookclub

import Exercises._
import Exercises.List._
import org.specs2.mutable.Specification

object ExercisesSpec extends Specification {
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

  "foldLeft" should {
    "reverse the list" in {
      foldLeft(List(1,2,3), Nil:List[Int])((b, a) => Cons(a, b)) === List(3,2,1)
    }
    "add the numbers" in {
      foldLeft(List(1,2,3), 0)(_ + _) === 6
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

  "append" should {
    "append an element to the end of a list" in {
      append(Nil, 1) === List(1)
      append(List(1, 2, 3), 4) === List(1, 2, 3, 4)
    }
    "append an element to the end of a list (using foldRight)" in {
      appendR(Nil, 1) === List(1)
      appendR(List(1, 2, 3), 4) === List(1, 2, 3, 4)
    }
    "append an element to the end of a list (using foldLeft)" in {
      appendL(Nil, 1) === List(1)
      appendL(List(1, 2, 3), 4) === List(1, 2, 3, 4)
    }
  }

  "reverse" should {
    "reverse any list (using foldLeft)" in {
      reverse(List(0, 1, 2)) === List(2, 1, 0)
      reverse(List(1)) === List(1)
      reverse(Nil) === Nil
    }
    "reverse any list (using foldRight)" in {
      reverseR(List(0, 1, 2)) === List(2, 1, 0)
      reverseR(List(1)) === List(1)
      reverseR(Nil) === Nil
    }
  }

  "the folds in terms of each other" should {
    "be able to do the same things as foldLeft" in {
      foldLeftR(List(1,2,3), Nil:List[Int])((b, a) => Cons(a, b)) === List(3,2,1)
      foldLeftR(List(1,2,3), 0)(_ + _) === 6
    }
    "be able to do the same things as foldRight" in {
      foldRightL(List(1,2,3), Nil:List[Int])(Cons(_,_)) === List(1,2,3)
    }
  }

  "concatList" should {
    "have a proper helper function" in {
      combineLists(List(1,2), List(3,4,5)) === List(1,2,3,4,5)
    }
    "combine the lists of lists into one list in linear time" in {
      concatList(List(List(1,2), List(3), Nil, List(4,5,6))) === List(1,2,3,4,5,6)
    }
  }
}

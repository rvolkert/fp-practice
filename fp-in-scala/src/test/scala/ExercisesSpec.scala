package rvolkert.bookclub

import Exercises._
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
}

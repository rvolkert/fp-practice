package rvolkert.bookclub

import rvolkert.bookclub.RNG._
import org.specs2.mutable.Specification

object Chapter6Spec extends Specification {
  def isPositiveAndLessThanOne(d: Double) = {
    d >= 0 && d < 1 === true
  }

  "nonNegativeInt" should {
    "generate a random integer between 0 and Int.maxValue (inclusive)" in {
      val gen = SimpleRNG(1)
      nonNegativeInt(gen) === nonNegativeInt(gen)
      nonNegativeInt(gen)._1 >= 0 === true

      val fakeGen = ConstantRNG(Int.MinValue)
      nonNegativeInt(fakeGen)._1 === 0

      val fakeGen1 = ConstantRNG(Int.MinValue + 1)
      nonNegativeInt(fakeGen1)._1 === Int.MaxValue

      val fakeGen2 = ConstantRNG(-1)
      nonNegativeInt(fakeGen2)._1 === 1

      val fakeGen3 = ConstantRNG(1)
      nonNegativeInt(fakeGen2)._1 === 1
    }
  }

  "double" should {
    "generate a Double between 0 and 1, not including 1" in {
      val gen = SimpleRNG(1)
      val d = double(gen)
      d === double(gen)
      isPositiveAndLessThanOne(d._1)

      isPositiveAndLessThanOne(double(ConstantRNG(-5))._1)
      isPositiveAndLessThanOne(double(ConstantRNG(Int.MinValue))._1)
      isPositiveAndLessThanOne(double(ConstantRNG(Int.MaxValue))._1)
      isPositiveAndLessThanOne(double(ConstantRNG(10))._1)
    }
  }

  "intDouble" should {
    "generate an int and a double from different rng states" in {
      val rng = SimpleRNG(100)
      val id = intDouble(rng)._1
      double(ConstantRNG(id._1)) !== id._2
    }
  }

  "doubleInt" should {
    "generate a double and an int from different rng states" in {
      val rng = SimpleRNG(100)
      val di = doubleInt(rng)
      double(ConstantRNG(di._1._2)) !== di._1._1
    }
  }

  "double3" should {
    "generate 3 different doubles" in {
      val rng = SimpleRNG(100)
      val d3 = double3(rng)._1
      d3._1 !== d3._2
      d3._2 !== d3._3
    }
  }

  "ints" should {
    "generate a list of ints" in {
      val rng = SimpleRNG(100)
      ints(5)(rng) === ints(5)(rng)
      ints(3)(ConstantRNG(5))._1 === List(5,5,5)
    }
  }

  "doubleMap" should {
    "generate a Double between 0 and 1, not including 1" in {
      val gen = SimpleRNG(1)
      val d = doubleMap(gen)
      d === doubleMap(gen)
      isPositiveAndLessThanOne(d._1)

      isPositiveAndLessThanOne(doubleMap(ConstantRNG(-5))._1)
      isPositiveAndLessThanOne(doubleMap(ConstantRNG(Int.MinValue))._1)
      isPositiveAndLessThanOne(doubleMap(ConstantRNG(Int.MaxValue))._1)
      isPositiveAndLessThanOne(doubleMap(ConstantRNG(10))._1)
    }
  }

  "ints2" should {
    "generate a list of ints" in {
      val rng = SimpleRNG(100)
      ints2(5)(rng) === ints2(5)(rng)
      ints2(3)(ConstantRNG(5))._1 === List(5,5,5)
    }
  }

  "map2" should {
    "lift a function f: (A, B) -> C into the realm of rng actions (RNG -> (X, RNG))" in {
      val randC: Rand[Int] = map2(_.nextInt, _.nextInt)(_ + _)
      randC(ConstantRNG(4))._1 === 8
      randC(ConstantRNG(4))._1 === 8
    }
    "aka, take two rng actions and combine them into a third action" in {
      val randC: Rand[Int] = map2(_.nextInt, _.nextInt)(_ + _)
      randC(ConstantRNG(4))._1 === 8
      randC(ConstantRNG(4))._1 === 8
    }
  }

  "randIntDouble" should {
    "generate an int and a double from different rng states" in {
      val rng = SimpleRNG(100)
      val id = randIntDouble(rng)._1
      double(ConstantRNG(id._1)) !== id._2
    }
  }

  "sequence" should {
    "make a list of Rand[A] into a Rand[List[A]], combining them into one transaction" in {
      val rng = SimpleRNG(100)
      val rand: Rand[Int] = _.nextInt
      sequence(List(rand, rand, rand, rand))(rng) === sequence(List(rand, rand, rand, rand))(rng)
      sequence(List(rand, rand, rand))(ConstantRNG(5)) === (List(5,5,5), ConstantRNG(5))
    }
  }

  "NestableFakeRNG" should {
    "return the next int and the rng it has" in {
      val rng = NestableFakeRNG(1, NestableFakeRNG(2, ConstantRNG(3)))
      val (one, rng2) = rng.nextInt
      val (two, rng3) = rng2.nextInt
      val (three, rng4) = rng3.nextInt
      val (three2, _) = rng4.nextInt
      one === 1
      two === 2
      three === 3
      three2 === 3
    }
  }

  "nonNegativeLessThan" should {
    "generates an integer between 0 (inclusive) and n (exclusive) - book version" in {
      val gen = NestableFakeRNG(Int.MaxValue, NestableFakeRNG(Int.MaxValue - 1, ConstantRNG(99)))
      nonNegativeLessThanBook(100)(gen)._1 === 99

      nonNegativeLessThanBook(100)(ConstantRNG(-99))._1 === 99
      nonNegativeLessThanBook(100)(ConstantRNG(0))._1 === 0
    }
    "generates an integer between 0 (inclusive) and n (exclusive) - flatMap version" in {
      val gen = NestableFakeRNG(Int.MaxValue, NestableFakeRNG(Int.MaxValue - 1, ConstantRNG(99)))
      nonNegativeLessThan(100)(gen)._1 === 99

      nonNegativeLessThan(100)(ConstantRNG(-99))._1 === 99
      nonNegativeLessThan(100)(ConstantRNG(0))._1 === 0
    }
  }

  // "mapWithFlatMap" in {
  //   mapWithFlatMap(nonNegativeInt)(i => i - i % 2)(ConstantRNG(-5))._1 === 4
  // }
  //
  // "map2WithFlatMap" in {
  //   map2WithFlatMap(int, int)((_, _))(NestableFakeRNG(1, ConstantRNG(2)))._1 === (1,2)
  // }
}

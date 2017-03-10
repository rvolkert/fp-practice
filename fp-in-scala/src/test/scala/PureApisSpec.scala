package rvolkert.fp.api
import org.specs2.mutable.Specification
import rvolkert.fp.api.PurelyRandom._
import org.specs2.specification.Scope

object PureApisSpec extends Specification {

  "nonNegativeInt" should {
    "generate a random integer between 0 and Int.maxValue (inclusive)" in {
      val gen = SimpleRNG(1)
      nonNegativeInt(gen) === nonNegativeInt(gen)
      nonNegativeInt(gen)._1 >= 0 === true

      val fakeGen = FakeRNG(Int.MinValue)
      nonNegativeInt(fakeGen)._1 === 0

      val fakeGen2 = FakeRNG(-1)
      nonNegativeInt(fakeGen2)._1 === 1

      val fakeGen3 = FakeRNG(1)
      nonNegativeInt(fakeGen2)._1 === 1
    }
  }

  "double" should {
    "generate a Double between 0 and 1, not including 1" in new context {
      val gen = SimpleRNG(1)
      val d = double(gen)
      d === double(gen)
      isPositiveAndLessThanOne(d._1)

      isPositiveAndLessThanOne(double(FakeRNG(-5))._1)
      isPositiveAndLessThanOne(double(FakeRNG(Int.MinValue))._1)
      isPositiveAndLessThanOne(double(FakeRNG(Int.MaxValue))._1)
      isPositiveAndLessThanOne(double(FakeRNG(10))._1)
    }
  }

  "intDouble" should {
    "generate an int and a double from different rng states" in {
      val rng = SimpleRNG(100)
      val id = intDouble(rng)._1
      double(FakeRNG(id._1)) !== id._2
    }
  }

  "doubleInt" should {
    "generate a double and an int from different rng states" in {
      val rng = SimpleRNG(100)
      val di = doubleInt(rng)
      double(FakeRNG(di._1._2)) !== di._1._1
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
    "generate a list of random ints" in {
      val rng = SimpleRNG(100)
      ints(5)(rng)._1.toSet.size === 5
    }
  }

  "intsSequence" should {
    "generate a list of random ints" in {
      val rng = SimpleRNG(100)
      intsSequence(5)(rng)._1.toSet.size === 5
    }
  }

  "doubleMap" should {
    "generate a Double between 0 and 1, not including 1" in new context {
      val gen = SimpleRNG(1)
      val d = doubleMap(gen)
      d === doubleMap(gen)
      isPositiveAndLessThanOne(d._1)

      isPositiveAndLessThanOne(doubleMap(FakeRNG(-5))._1)
      isPositiveAndLessThanOne(doubleMap(FakeRNG(Int.MinValue))._1)
      isPositiveAndLessThanOne(doubleMap(FakeRNG(Int.MaxValue))._1)
      isPositiveAndLessThanOne(doubleMap(FakeRNG(10))._1)
    }
  }

  "randIntDouble" should {
    "generate an int and a double from different rng states" in {
      val rng = SimpleRNG(100)
      val id = randIntDouble(rng)._1
      double(FakeRNG(id._1)) !== id._2
    }
  }

  "randDoubleInt" should {
    "generate a double and an int from different rng states" in {
      val rng = SimpleRNG(100)
      val di = randDoubleInt(rng)
      double(FakeRNG(di._1._2)) !== di._1._1
    }
  }

  "nonNegativeLessThan" should {
    "generates an integer between 0 (inclusive) and n (exclusive)" in {
      val gen = NestableFakeRNG(Int.MaxValue, NestableFakeRNG(Int.MaxValue - 1, FakeRNG(99)))
      nonNegativeLessThan(100)(gen)._1 === 99

      nonNegativeLessThan(100)(FakeRNG(-99))._1 === 99
      nonNegativeLessThan(100)(FakeRNG(0))._1 === 0
    }
  }

  "mapWithFlatMap" in {
    mapWithFlatMap(nonNegativeInt)(i => i - i % 2)(FakeRNG(-5))._1 === 4
  }

  "map2WithFlatMap" in {
    map2WithFlatMap(int, int)((_, _))(NestableFakeRNG(1, FakeRNG(2)))._1 === (1,2)
  }

  trait context extends Scope {
    def isPositiveAndLessThanOne(d: Double) = {
      d >= 0 && d < 1 === true
    }
  }
}

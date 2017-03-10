package rvolkert.fp.api

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

case class FakeRNG(i: Int) extends RNG {
  def nextInt: (Int, RNG) = (i, this)
}

case class NestableFakeRNG(i: Int, next: RNG) extends RNG {
  def nextInt: (Int, RNG) = (i, next)
}


object PurelyRandom {
  /*EXERCISE 6.1
  Write a function that uses RNG.nextInt to generate a random integer between 0 and
  Int.maxValue (inclusive). Make sure to handle the corner case when nextInt returns
  Int.MinValue, which doesn’t have a non-negative counterpart.
  */
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, nextRNG) = rng.nextInt
      ({if (i > Int.MinValue) Math.abs(i) else 0}, nextRNG)
    }

  /*EXERCISE 6.2
Write a function to generate a Double between 0 and 1, not including 1. Note: You can
use Int.MaxValue to obtain the maximum positive integer value, and you can use
x.toDouble to convert an x: Int to a Double.
*/

 def double(rng: RNG): (Double, RNG) = {
   val (i, nextRNG) = rng.nextInt
   val divisor = if (i < 0) (Int.MinValue - 1) else (Int.MaxValue + 1)
   (i.toDouble / divisor, nextRNG)
 }

/*EXERCISE 6.3
Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
(Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve
already written.*/
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i,d), rng3)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), nextRNG) = intDouble(rng)
    ((d,i), nextRNG)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d,d2,d3), rng4)
  }
/*EXERCISE 6.4
Write a function to generate a list of random integers.*/
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, gen: RNG, acc: List[Int] = List()): (List[Int], RNG) = {
      if (count > 0) {
        val (i, nextGen) = gen.nextInt
        go(count - 1, nextGen, i :: acc)
      } else {
        (acc, gen)
      }
    }
    go(count, rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  /*EXERCISE 6.5
Use map to reimplement double in a more elegant way. See exercise 6.2.
 def double(rng: RNG): (Double, RNG) = {
   val (i, nextRNG) = rng.nextInt
   val divisor = if (i < 0) (Int.MinValue - 1) else (Int.MaxValue + 1)
   (i.toDouble / divisor, nextRNG)
 }
*/
  def doubleMap: Rand[Double] = {
    map(int){ i =>
      i.toDouble / (if (i < 0) (Int.MinValue - 1) else (Int.MaxValue + 1))
    }
  }

  /*EXERCISE 6.6
Write the implementation of map2 based on the following signature. This function
takes two actions, ra and rb, and a function f for combining their results, and returns
a new action that combines them:*/
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)


/*EXERCISE 6.7
Hard: If you can combine two RNG transitions, you should be able to combine a whole
list of them. Implement sequence for combining a List of transitions into a single
transition. Use it to reimplement the ints function you wrote before. For the latter,
you can use the standard library function List.fill(n)(x) to make a list with x
repeated n times.*/
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      def go(fs: List[Rand[A]], gen: RNG, acc: List[A] = List()): (List[A], RNG) = fs match {
        case h :: t =>
          val (a, nextGen) = h(gen)
          go(t, nextGen, a :: acc)
        case Nil =>
          (acc, gen)
      }
      go(fs, rng)
    }
  }

  def intsSequence(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  /*EXERCISE 6.8
Implement flatMap, and then use it to implement nonNegativeLessThan.*/
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    //the conditional ensures an equal chance for all numbers less than n to show up by removing the highest ints
    //between the highest possible Int multiple of n and MaxValue
    flatMap(nonNegativeInt) { i => {
      rng => {
        val mod = i % n
        if (i + (n-1) - mod >= 0) (mod, rng) else nonNegativeLessThan(n)(rng)
      }
    }}
  }

  /*EXERCISE 6.9
Reimplement map and map2 in terms of flatMap. The fact that this is possible is what
we’re referring to when we say that flatMap is more powerful than map and map2.*/

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B) =
    flatMap(s){ a => rng => (f(a), rng) }

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra){ a =>
    rng => {
      val (b, rng2) = rb(rng)
      (f(a,b), rng2)
    }
  }

  // type State[S,+A] = S => (A,S)
  case class State[S,+A](run: S => (A,S)) {
    def unit[B >: A](b: B): State[S,B] =
      State(s => (b, s))

    def map[B](f: A => B): State[S, B] = {
      State(s => {
        val (a, s2) = run(s)
        (f(a), s2)
      })
    }

    def flatMap[B](f: A => State[S,B]): State[S,B] =
      State(s => {
        val (a, s2) = run(s)
        f(a).run(s2)
      })
  }

  /*EXERCISE 6.10
Generalize the functions unit, map, map2, flatMap, and sequence. Add them as methods
on the State case class where possible. Otherwise you should put them in a State
companion object.*/
}

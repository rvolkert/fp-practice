package rvolkert.bookclub


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  case class ConstantRNG(constant: Int) extends RNG {
    def nextInt: (Int, RNG) = (constant, this)
  }

  case class NestableFakeRNG(i: Int, next: RNG) extends RNG {
    def nextInt: (Int, RNG) = (i, next)
  }

  // type alias for the RNG "state action" or "state transition" data type
  // it is a program that takes an RNG to generate an A and transition the RNG to a new state
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // EXERCISE 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, nextRNG) = rng.nextInt
    val y =
      if (x >= 0)
        x
      else if (x == Int.MinValue)
        0
      else
        x * -1
    (y, nextRNG)
  }

  // 6.2 double in [0,1)
  def double(rng: RNG): (Double, RNG) = {
    val (x, nextRNG) = nonNegativeInt(rng)
    ((x - 1).toDouble / Int.MaxValue, nextRNG)
  }

  // 6.3 reuse previous functions to write intDouble, doubleInt, and double3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i,d), rng3)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d,i), rng3)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1,d2,d3), rng4)
  }

  // 6.4 list of random ints
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    // if (count <= 0)
    //   (Nil, rng)
    // else {
    //   val (x, rng2) = rng.nextInt
    //   val (xs, rng3) = ints(count - 1)(rng2)
    //   (x :: xs, rng3)
    // }
    def go(remaining: Int, gen: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (remaining <= 0) {
        (acc, gen)
      } else {
        val (x, gen2) = gen.nextInt
        go(remaining - 1, gen2, x :: acc) // note: these will come out backward of their rngs
      }
    }
    go(count, rng, Nil)
  }

  // 6.5 use map to implement double
  def doubleMap(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(x => (x - 1).toDouble / Int.MaxValue)(rng)
  }

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a,b), rng3)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra,rb)((_,_))
  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    def go(ops: List[Rand[A]], gen: RNG): (List[A], RNG) = ops match {
      case h :: t =>
        val (a, gen2) = h(rng)
        val (as, genX) = go(t, gen2)
        (a :: as, genX)
      case Nil =>
        (Nil, gen)
    }
    go(fs, rng)
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)({gen: RNG => gen.nextInt}))(rng)
  }

  // 6.8
  //from the book
  def nonNegativeLessThanBook(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    // if i is greater than the largest multiple of n
    // before Int.MaxValue, m, this will wrap around into the negatives
    // we don't want to use i, then, because nonNegativeLessThan's
    // results will be unevenly distributed in favor of values between
    // 0 and (Int.MaxValue - m)
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThanBook(n)(rng2)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        rng => (mod, rng)
      else nonNegativeLessThan(n)
    }
  }

  def mapWithFlatMap[A,C](f: Rand[A])(h: A => C): Rand[C] = {
    flatMap(f)(a => rng => (h(a), rng))
  }

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        rng => (f(a,b), rng)
      }
    }
  }
  //flatMap is considered more powerful than map and map2 because they can be written in terms of it
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap{ a: A => State(s => (f(a), s)) }

  def mapFor[B](f: A => B): State[S, B] =
    for {
      a <- this
    } yield f(a)

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a =>
      sb.flatMap { b =>
        State(s => (f(a,b), s))
      }
    }
  def mapTwo[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    for {
      a <- this
      b <- sb
    } yield {
      f(a,b)
    }
  }
  def flatMap[B](f: A => State[S, B]): State[S, B] = State{ s =>
    val (a, s2) = run(s)
    f(a).run(s2)
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

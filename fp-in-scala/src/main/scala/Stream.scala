package rvolkert.fp.stream

sealed trait Stream[+A] {
  import Stream._
  /*EXERCISE 5.1
Write a function to convert a Stream to a List, which will force its evaluation and let
you look at it in the REPL. You can convert to the regular List type in the standard
library. You can place this and other functions that operate on a Stream inside the
Stream trait.*/

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /*EXERCISE 5.2
Write the function take(n) for returning the first n elements of a Stream, and
drop(n) for skipping the first n elements of a Stream.*/

  def take(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) if n >= 1 => cons(h(), t().take(n-1))
    case c => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n >= 1 => t().drop(n-1)
    case c => c
  }

  /*EXERCISE 5.3
Write the function takeWhile for returning all starting elements of a Stream that
match the given predicate.*/

  def takeWhile(p: A => Boolean): Stream[A] = this match {//should I write inner functions for all of these to not call t()?
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)
    /*This definition of exists, though illustrative, isn’t stack-safe if the stream is large and all elements test false.*/

  /*EXERCISE 5.4
Implement forAll, which checks that all elements in the Stream match a given predicate.
Your implementation should terminate the traversal as soon as it encounters a nonmatching value.*/

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  /*EXERCISE 5.5
Use foldRight to implement takeWhile.*/

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Cons(() => a, () => b.takeWhile2(p)) else Empty)
  }

  /*EXERCISE 5.6//todo
Hard: Implement headOption using foldRight.*/
  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, opt) => Some(a))
  }

  /*EXERCISE 5.7
Implement map, filter, append, and flatMap using foldRight. The append method
should be non-strict in its argument.*/
  def map[B](f: A => B): Stream[B] = {
    foldRight(Empty: Stream[B])((a, b) => Cons(() => f(a), () => b))
  }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Cons(() => a, () => b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => Cons(() => a, () => b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => f(a).append(b))
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

object StreamRecursion {
  import Stream._
  /*Try playing with a few other examples:
ones.map(_ + 1).exists(_ % 2 == 0)
ones.takeWhile(_ == 1)
ones.forAll(_ != 1)*/
  val ones: Stream[Int] = Stream.cons(1, ones)

  /*EXERCISE 5.8
Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.*/

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  /*EXERCISE 5.9
Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on.
- It’s possible to define a stack-safe version of forAll using an ordinary recursive loop.
- In Scala, the Int type is a 32-bit signed integer, so this stream will switch from positive to negative values at
some point, and will repeat itself after about four billion elements.*/

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  /*EXERCISE 5.10
Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.*/
  def fibs(): Stream[Int] = {
    def go(first: Int, second: Int): Stream[Int] = {
      Stream.cons(first, Stream.cons(second, go(first + second, first + 2 * second)))
    }
    go(0,1)
  }

  /*EXERCISE 5.11
Write a more general stream-building function called unfold. It takes an initial state,
and a function for producing both the next state and the next value in the generated stream.*/

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Stream.empty
      case Some((head, newState)) => Stream.cons(head, unfold(newState)(f))
    }
  }

  /*ask about: 8 Using unfold to define constant and ones means that we don’t get sharing as in the recursive definition
val ones: Stream[Int] = cons(1, ones). The recursive definition consumes constant memory even if we
keep a reference to it around while traversing it, while the unfold-based implementation does not. Preserv-
ing sharing isn’t something we usually rely on when programming with streams, since it’s extremely delicate
and not tracked by the types. For instance, sharing is destroyed when calling even xs.map(x => x).*/

  /*EXERCISE 5.12
Write fibs, from, constant, and ones in terms of unfold.*/

  def fibs2(): Stream[Int] = cons(0, cons(1, unfold((0,1)){case (x: Int, y: Int) => Some(((x+y), (y, x+y)))}))

  def from2(n: Int): Stream[Int] = unfold(n){ x => Some((x, x + 1)) }

  def constant2[A](a: A): Stream[A] = unfold(a){ x => Some((x, x)) }

  def ones2: Stream[Int] = unfold(1){_ => Some((1, 1))}

  /*EXERCISE 5.13
Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and
zipAll. The zipAll function should continue the traversal as long as either stream
has more elements—it uses Option to indicate whether each stream has been
exhausted.

zipWith: Write a function that accepts two lists and constructs a new list by doing the operation on the correspond-
ing elements. For example, List(1,2,3) and List(4,5,6) with _ + _ become List(5,7,9).*/

  def mapU[A, B](stream: Stream[A])(f: A => B): Stream[B] = {
    unfold(stream){ s => s match {
      case Cons(head, tail) => Some((f(head()), tail()))
      case Empty => None
    }}
  }

  def takeU[A](stream: Stream[A])(n: Int): Stream[A] = {
    unfold((stream, n)){ case (s, n) => s match {
      case Cons(head, tail) if n > 0 => Some((head(), (tail(), n - 1)))
      case _ => None
    }}
  }

  def takeWhileU[A](stream: Stream[A])(p: A => Boolean): Stream[A] =
    unfold(stream){ s => s match {
      case Cons(head, tail) if p(head()) => Some(head(), tail())
      case _ => None
    }}

  def zipWith[A, B](stream: Stream[A], other: Stream[A])(f: (A, A) => B): Stream[B] =
    unfold((stream, other)){ case (s, o) => (s, o) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }}

  def zipAll[A, B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    unfold(s1, s2){ case (s1, s2) => (s1, s2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
      case _ => None
    }}
  }
  /*EXERCISE 5.14
Hard: Implement startsWith using functions you’ve written. It should check if one
Stream is a prefix of another. For instance, Stream(1,2,3) startsWith Stream(1,2)
would be true.*/
  // def startsWith[A](stream: Stream[A], other: Stream[A]): Boolean =

  /*EXERCISE 5.15
Implement tails using unfold. For a given Stream, tails returns the Stream of suffixes of the input sequence,
starting with the original Stream. For example, given
Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3),
Stream()).*/
  def tails[A](stream: Stream[A]): Stream[Stream[A]] =
    cons(stream, unfold(stream){ s => s match {
      case Cons(h, t) => Some(t(), t())
      case Empty => None
    }})
  /*EXERCISE 5.16
Hard: Generalize tails to the function scanRight, which is like a foldRight that
returns a stream of the intermediate results. For example:
scala> Stream(1,2,3).scanRight(0)(_ + _).toList
res0: List[Int] = List(6,5,3,0)
This example should be equivalent to the expression List(1+2+3+0, 2+3+0, 3+0,
0). Your function should reuse intermediate results so that traversing a Stream with n
elements always takes time linear in n. Can it be implemented using unfold? How, or
why not? Could it be implemented using another function we’ve written?*/
}

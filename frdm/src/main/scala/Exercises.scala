// Create your own Monoid Typeclass.
// Create instances of it for Int, String, and some case class that you create.

// Create a function that is paramaterized on A and folds over the input: List[A] while requiring a Monoid to exist for A.

// Write your own Foldable Typeclass.
// Create an instance of Foldable for List.
//
// Write your own Functor Typeclass.
// Create Functor instances for Option, List, and Either.


//
// Write your own Applicative Typeclass.
// Create Applicative instance for Option.
//
// Write your own Monad typeclass.
// Create Monad instances for Option and List.

package frdm.exercises

object Exercises {
  trait Monoid[A] {
    def zero: A
    def op(x: A, y: A): A
  }

  implicit def intAdditionMonoid: Monoid[Int] = new Monoid[Int]{
    def zero = 0
    def op(x: Int, y: Int): Int = x + y
  }

  implicit def intMultiplicationMonoid: Monoid[Int] = new Monoid[Int]{
    def zero = 1
    def op(x: Int, y: Int): Int = x * y
  }

  implicit def monoidString: Monoid[String] = new Monoid[String]{
    def zero = ""
    def op(x: String, y: String): String = x + y
  }

  case class Foo(m: Map[Int, String])

  implicit def monoidFoo: Monoid[Foo] = new Monoid[Foo]{
    def zero = Foo(Map[Int,String]())
    def op(x: Foo, y: Foo): Foo = Foo(x.m ++ y.m)
  }

  def fooFold[A](as: List[A])(implicit m: Monoid[A]): A = {
    as.fold(m.zero)(m.op)
  }
// *->*
  trait Foldable[F[_]] {
    def foldl[A,B](as: F[A], zero: B, f: (B, A) => B): B
    def foldMap[A,B](as: F[A])(f: A => B)(implicit m: Monoid[B]): B = {
      foldl(as, m.zero, (b: B, a: A) => m.op(b, f(a)))
    }
  }

  implicit def listFold = new Foldable[List] {
    def foldl[A,B](as: List[A], zero: B, f: (B, A) => B): B = {
      as.foldLeft(zero)(f)
    }
  }

  trait Functor[F[_]] {
    def map[A,B](as: F[A])(f: A => B): F[B]
  }

  implicit def optionFunctor = new Functor[Option] {
    def map[A,B](maybeA: Option[A])(f: A => B): Option[B] = {
      maybeA.map(f)
    }
  }

  implicit def listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = {
      as.map(f)
    }
  }

  trait Functor2[F[_,_]] {
    def map[A,B,C,D](as: F[A,B])(f: B => C)(g: A => D): F[D,C]
  }

  implicit def eitherFunctor = new Functor2[Either] {
    def map[A,B,C,D](aOrB: Either[A,B])(f: B => C)(g: A => D): Either[D,C] = {
      aOrB match {
        case Left(a) => Left(g(a))
        case Right(b) => Right(f(b))
      }
    }

    def mapR[A,B,C](aOrB: Either[A,B])(f: B => C): Either[A,C] = {
      map(aOrB)(f)(identity)
    }
  }

  trait Applicative[F[_]] extends Functor[F] {
    def ap[A,B](fa: => F[A])(f: => F[A => B]): F[B]

    def apply2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      ap(fb)(map(fa)(f.curried))
    }
  }

  implicit def optionApplicative = new Applicative[Option] {
    def map[A,B](maybeA: Option[A])(f: A => B): Option[B] = maybeA.map(f) //todo: ask if there's a better way
    def ap[A,B](fa: => Option[A])(f: => Option[A => B]): Option[B] = {// => is a "call by name"
      // map(fa)(a => map(f)(g => g(a))).flatten
      (fa, f) match {
        case (Some(a), Some(g)) => Some(g(a))
        case _ => None
      }
    }
  }

  trait Monad[F[_]] extends Applicative[F] {
    def flatmap[A,B](fa: F[A])(f: A => F[B]): F[B]
  }

  implicit def optionMonad = new Monad[Option] {
    def map[A,B](maybeA: Option[A])(f: A => B): Option[B] = maybeA.map(f) //todo: ask if there's a better way
    def ap[A,B](fa: => Option[A])(f: => Option[A => B]): Option[B] = ???
    def flatmap[A,B](fa: Option[A])(f: A => Option[B]): Option[B] = {
      fa match {
        case Some(a) => f(a)
        case None => None
      }
    }
  }

  implicit def listMonad = new Monad[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = {
      as.map(f)
    }
    def ap[A,B](fa: => List[A])(fun: => List[A => B]): List[B] = {
      for {
        a <- fa
        f <- fun
      } yield {
        f(a)
      }
    }

    def flatmap[A,B](fa: List[A])(f: A => List[B]): List[B] = {
      fa.flatMap(f)
    }
  }

    import scalaz.State
    import State._

  //   State[S, +A] {
  // def apply(s: S): (S, A)}

    // implicit def counter = new State[Int, Boolean] {
      // def apply(i: Int): (Int, Boolean) = {
      //   (i + 1, i > 10)
      // }
    // }
  // }
}

//
// Applicative vs Monad what is the difference when doing computations?
//
// Import Cats or Scalaz State, play around with it.
//
// Import Kleisli.  Create example with Repository => M[A]
//
// Create Example of how DSLs are created with Free Monads in Cats. Use the wiki or make your own.

package rvolkert.bookclub

import rvolkert.bookclub.Exercises.{List, Nil, Cons}
import rvolkert.bookclub.Exercises.List._

object Chapter4 {
  sealed trait Option[+A] {
    // Exercise 4.1
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    // B is a supertype of A
    // => B only evaluates b when called (non-strict, lazy)
    def getOrElse[B >: A](b: => B): B = this match {
      case None => b
      case Some(a) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      this.map(f(_)).getOrElse(None)
    }

    def orElse[B >: A](default: => Option[B]): Option[B] = {
      this.map(Some(_)).getOrElse(default)
    }

    def filter(f: A => Boolean): Option[A] = {
      this.flatMap(a => if (f(a)) Some(a) else None)
    }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  //from the book
  //making a function able to operate on Options, explicitly
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  object Option {
    // Exercise 4.3
    def map2[A,B,C](maybeA: Option[A], maybeB: Option[B])(f: (A, B) => C): Option[C] = {
      maybeA.flatMap(a => maybeB.map(b => f(a,b)))
    }

    // Exercise 4.4
    /*Write a function sequence that combines a list of Options into one Option containing
    a list of all the Some values in the original list. If the original list contains None even
    once, the result of the function should be None; otherwise the result should be Some
    with a list of all the values. Here is its signature:3*/
    // def sequence[A](maybeAs: List[Option[A]]): Option[List[A]] = maybeAs match {
    //   case Nil => None
    //   case Cons(h,t) =>
    //     foldLeft(t, h.map(List(_)))((acc, maybeA) =>
    //       acc.flatMap(as => maybeA.map(a => Cons(a, as)))
    //     ).map(reverse(_))
    // }
    def sequence[A](maybeAs: List[Option[A]]): Option[List[A]] = {
      // keep replacing the acc to find all `as`
      foldRightL(maybeAs, Some(List()):Option[List[A]]){ (maybeA, acc) =>
        map2(maybeA, acc)(Cons(_, _))
      }
    }

    // Exercise 4.5
    def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
      foldRightL(as, Some(List()):Option[List[B]]){ (a, acc) =>
        map2(f(a), acc)(Cons(_, _))
      }
    }
    // def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
    //   case Nil => None
    //   case Cons(h,t) =>
    //     foldLeft(t, f(h).map(List(_)))((acc, a) =>
    //       acc.flatMap(bs => f(a).map(b => Cons(b, bs)))
    //     ).map(reverse(_))
    // }

    def sequence2[A](maybeAs: List[Option[A]]): Option[List[A]] = {
      traverse(maybeAs)(identity)
    }
  }

  sealed trait Either[+E, +A] {
    // Exercise 4.6
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

    // helper method I added that also accounts for the e if it's needed for the default
    def getOrElse[B >: A](f: E => B): B = this match {
      case Right(a) => a
      case Left(e) => f(e)
    }

    // def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    //   case Right(a) => f(a)
    //   case Left(e) => Left(e)
    // }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      this.map(a => f(a)).getOrElse(Left(_))
    }

    // def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    //   case Right(a) => Right(a)
    //   case left => b
    // }
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
      this.map(a => Right(a)).getOrElse(_ => b)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this.flatMap(aa => b.map(bb => f(aa, bb)))
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {
    def Try[A](a: => A): Either[Exception, A] =
      try Right(a)
      catch { case e: Exception => Left(e) }

    // Exercise 4.7
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
      // keep replacing the acc to find all `as`
      foldRightL(es, Right(List()):Either[E,List[A]]){ (either, acc) =>
        either.map2(acc)(Cons(_, _))
      }
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      foldRightL(as, Right(List()): Either[E, List[B]]){ (a, acc) =>
        f(a).map2(acc)(Cons(_, _))
      }
    }

    def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
      traverse(es)(identity)
    }
  }
}

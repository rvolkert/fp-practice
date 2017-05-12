package rvolkert.bookclub

import scala.collection.immutable.{Stream => _, _}

object Chapter5 {
  trait Stream[+A] {
    import Stream._

    //5.1
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
      this match {
        case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
        case _ => z
      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

    @annotation.tailrec
    final def find(f: A => Boolean): Option[A] = this match {
      case Empty => None
      case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
    }
    def take(n: Int): Stream[A] = {//todo: rewrite without list
      @annotation.tailrec
      def go(as: Stream[A], acc: List[A], x: Int): List[A] = as match {
        case Cons(h, t) if x > 0 => go(t(), h() :: acc, x - 1)
        case _ => acc //if there aren't n items in the stream, just return as many as we can
      }
      toStream(go(this, Nil, n).reverse)
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = {
      @annotation.tailrec
      def go(as: Stream[A], acc: List[A]): List[A] = as match {
        case Cons(h, t) if p(h()) => go(t(), h() :: acc)
        case _ => acc
      }
      toStream(go(this, Nil).reverse)
    }

    def takeWhile2(p: A => Boolean): Stream[A] = {
      //remember we're folding right, so we need to put empty in the acc to override anything that matched on the right before
      this.foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty[A])
    }

    def forAll(p: A => Boolean): Boolean = this match {
      case Empty => true
      case Cons(h, t) if !p(h()) => false
      case Cons(h, t) => t().forAll(p)
    }

    def forAll2(p: A => Boolean): Boolean = {
      foldRight(true)((a,b) => p(a) && b)
    }

    def headOption: Option[A] = {
      foldRight(None:Option[A])((a,b) => Some(a))
    }

    // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
    // writing your own function signatures.

    def map[B](f: A => B): Stream[B] = {
      foldRight(empty[B])((a, b) => cons(f(a), b))
    }

    def filter(f: A => Boolean): Stream[A] = {
      foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)
    }

    def append[B >: A](b: => B): Stream[B] = {
      foldRight(cons(b, empty))((a, acc) => cons(a, acc))
    }

    def appendList[B >: A](bs: => Stream[B]): Stream[B] = {
      foldRight(bs)((a, acc) => cons(a, acc))
    }

    def flatMap[B](f: A => Stream[B]): Stream[B] = {
      // foldRight(empty[B])((a,bs) => f(a).foldRight(bs)((b, acc) => cons(b, acc))) ===
      foldRight(empty[B])((a,bs) => f(a).appendList(bs))
    }

    // 5.14
    def startsWith[B >: A](s: Stream[B]): Boolean = {//try this with takeWhile and forAll
      zipAll(this, s).foldRight(false){(as, acc) =>
        val (maybeA, maybeExpected) = as
        (maybeA, maybeExpected) match {
          case (Some(a), Some(expected)) => if (a == expected) acc else false
          case (_, None) => true
          case (None, _) => false
      }}
    }
    // unfold((this, s)){
    //   case (Cons(h,t), Cons(expected, remaining)) => Some( ( h() == expected(), (t(), remaining())) )
    //   case (_, Empty) => None
    // } == constant(true)//compare to see if they're all trues

    def hasSubsequence[B >: A](s: Stream[B]): Boolean = {
      tails(this) exists (_ startsWith s)
    }

/*EXERCISE 5.16
Hard: Generalize tails to the function scanRight, which is like a foldRight that
returns a stream of the intermediate results. For example:
scala> Stream(1,2,3).scanRight(0)(_ + _).toList
res0: List[Int] = List(6,5,3,0)
This example should be equivalent to the expression List(1+2+3+0, 2+3+0, 3+0,
0). Your function should reuse intermediate results so that traversing a Stream with n
elements always takes time linear in n. Can it be implemented using unfold? How, or
why not? Could it be implemented using another function we’ve written?*/

    // def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = { //currently backwards
    //   unfold((this, z, false)){ ss => (ss._1, ss._2, ss._3) match {
    //     case (Cons(h,t), acc, _) => Some((acc, (t(), f(h(), acc), false)))
    //     case (Empty, acc, false) => Some((acc, (empty, acc, true)))
    //     case (Empty, _, true) => None
    //   }}
    // }
    //
    // def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    //   def go(z: B): (Stream[B], B) =
    //     tails(this).foldRight((empty, z)){(s, b) =>
    //       val (sAcc, acc) = b
    //       s.foldRight(acc)(f)
    //     }
    // }
    // def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    //   def previous(s: Stream[A]): Stream
    //   def go(remaining: Stream[A]): Stream[B] = {
    //     remaining match {
    //       case Cons(h,t) =>
    //         val prev = previous(t())
    //         prev match {
    //           case Cons(b, bs) =>
    //             cons(f(h(), b), prev)
    //           case Empty =>
    //             cons
    //         }
    //       case Empty => Stream(acc)
    //     }
    //   }
    //   go(this, Stream(), z)
    // }
    // def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    //   //try building up a function out of f and the previous in another internal function so you can reuse previous results
    //   this match {
    //     case Empty =>
    //       Stream(z)
    //     case Cons(h, Empty) =>
    //
    //     case Cons(h, t) =>
    //       scanRight()
    //   }
    // }

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
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = Stream.cons(1, ones)

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    def fibs: Stream[Int] = {
      def go(x: Int, y: Int): Stream[Int] = {
        cons(x, go(y, x + y))
      }
      go(0, 1)
    }

    // this is a corecursive function (said to have guarded recursion), which produces data
    // and doesn't need to termintate as long as it is productive (coterminates),
    // that is, we can evaluate more of the result in a finite amount of time
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z).fold(empty[A])({case (a, s) => cons(a, unfold(s)(f))})
    }

    // a re-write of the functions in terms of unfold; 5.13
    val ones2: Stream[Int] = unfold({})(_ => Some((1, {})))
    def constant2[A](z: A): Stream[A] = unfold(z)((a: A) => Some((a, a)))
    def from2(n: Int): Stream[Int] = unfold(n)((x: Int) => Some((x, x + 1)))
    def fibs2: Stream[Int] = {
      unfold((0,1))({case (first, second) => Some((first, (second, first + second)))})
    }

    def mapU[A,B](s: Stream[A])(f: A => B): Stream[B] = {
      unfold(s)(_ match {
        case Cons(h,t) => Some((f(h()), t()))
        case Empty => None
      })
    }

    def takeU[A](s: Stream[A])(n: Int): Stream[A] = {
      unfold((s, n)){ case (stream, x) => stream match {
        case Cons(h,t) if x > 0 => Some((h(), (t(), x - 1)))
        case _ => None
      }}
    }

    def takeWhileU[A](s: Stream[A])(p: A => Boolean): Stream[A] = {
      unfold(s)(_ match {
        case Cons(h,t) =>
          val a = h() // to not evaluate h twice
          if (p(a)) Some((a, t())) else None
        case Empty =>
          None
      })
    }

    def zipWith[A,B](s1: Stream[A], s2: Stream[A])(f: (A, A) => B): Stream[B] = {
      unfold((s1, s2)){ ss => (ss._1, ss._2) match {
        case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(), h2()), (t1(), t2())))
        case _ => None
      }}
    }

    def zipAll[A,B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A], Option[B])] = {
      unfold((s1, s2)){ ss => (ss._1, ss._2) match {
        case (Cons(h1,t1), Cons(h2,t2)) => Some( ( (Some(h1()), Some(h2())), (t1(), t2()) ) )
        case (Empty, Cons(h2,t2)) => Some( ( (None, Some(h2())), (empty, t2()) ) )
        case (Cons(h1,t1), Empty) => Some( ( (Some(h1()), None), (t1(), empty) ) )
        case _ => None
      }}
    }

    //all the tails of s, starting with s and ending with empty stream
    def tails[A](stream: Stream[A]): Stream[Stream[A]] = {
      unfold((stream, false)){ ss => (ss._1, ss._2) match {
        case (Cons(h,t), _) => Some((ss._1, (t(), false)))
        case (Empty, false) => Some((empty, (empty, true)))
        // case (Empty, true) => None
        case _ => None // fixes compile error about exhaustivity, though it was exhaustive before
      }}
    }
//____________used above as helper
    def toStream[A](as: List[A]): Stream[A] = as match {
      case Nil => Empty
      case h :: t => cons(h, toStream(t))
    }
  }
}

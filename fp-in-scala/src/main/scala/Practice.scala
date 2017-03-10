package rvolkert.fp

object Practice {
  //Exercise 2.1
  def fib(n: Int): Int = {
    //assuming n will always be positive, non-zero
    @annotation.tailrec
    def loop(twoPrevious: Int, onePrevious: Int, countDown: Int): Int = {
      if (countDown <= 1) twoPrevious
      else loop(onePrevious, twoPrevious + onePrevious, countDown - 1)
    }
    loop(0, 1, n)
  }
/*
1 2 3 4 5 6 7
0 1 1 2 3 5 8
*/

  //Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(index: Int, acc: Boolean): Boolean = {
      if (index + 1 >= as.length) acc
      else loop(index + 1, acc && ordered(as(index), as(index + 1)))
    }
    loop(0, true)
  }//try without acc -> return the result

  //Exercise 2.3 - return function literal that matches the type signature
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    //a => f(a, _)
    a => b => f(a, b)
  }

  //Exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  //Exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}

//from the book Functional Programming in Scala
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //Exercise 3.1
  //What will be the result of the following match expression?
  //3
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  //Exercise 3.2
  /*Implement the function tail for removing the first element of a List. Note that the
  function takes constant time. What are different choices you could make in your
  implementation if the List is Nil? We’ll return to this question in the next chapter.*/
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil//could have thrown an error, but tail claims to support all List[A]s; then again, Nil.tail throws an error in the REPL
    case Cons(_, t) => t
  }

  //Exercise 3.3
  /*Using the same idea, implement the function setHead for replacing the first element
  of a List with a different value.*/
  def setHead[A](as: List[A], newHead: A): List[A] = as match {
    case Nil => Nil//could have done Cons(newHead, Nil), but this changes the length of the list, and Nil.head throws an error
    case Cons(_, t) => Cons(newHead, t)
  }

  //Exercise 3.4
  /*Generalize tail to the function drop, which removes the first n elements from a list.
  Note that this function takes time proportional only to the number of elements being
  dropped—we don’t need to make a copy of the entire List.
  def drop[A](l: List[A], n: Int): List[A]*/
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case l if n <= 0 => l
    case Cons(_, t) => drop(t, n-1)
  }

  //Exercise 3.5
  /*Implement dropWhile, which removes elements from the List prefix as long as they
  match a predicate.*/
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case l => l
  }

  //Exercise 3.6
  /*Implement a function, init, that returns a List
  consisting of all but the last element of a List. So, given List(1,2,3,4), init will
  return List(1,2,3). Why can’t this function be implemented in constant time like tail? because
  of the way List is constructed, with the elements individually prepended to the rest of the list, recursively--
  there's no way to get at just the last element without traversing the whole thing*/
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  /*Exercise 3.7
  Can product, implemented using foldRight, immediately halt the recursion and
  return 0.0 if it encounters a 0.0? Why or why not?
  no, there's no check for any particular element being in the list in foldRight's implementation
  you evaluate everything before combining everything anyway, too

  Consider how any short-circuiting
  might work if you call foldRight with a large list. This is a deeper question that we’ll
  return to in chapter 5.
  you could build in a check for the zero element and have another case with an argument of what to do*/

  /*
  Exercise 3.8
  See what happens when you pass Nil and Cons themselves to foldRight, like this:
  foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).
  it reconstructs the list
  What do you think this says about the relationship between foldRight and the data constructors of List (Nil and Cons)?
  Cons is the identity function and Nil is the identity element of foldRight - ask D6
  */

  /*Exercise 3.9
  Compute the length of a list using foldRight.*/
  def lengthOfList[A](as: List[A]): Int =
    foldRight(as, 0)((x,y) => 1 + y)

  /*Exercise 3.10
  Our foldRight is not stack-safe. Write another general list-recursion function, foldLeft, that is
  tail-recursive, using the techniques we discussed in the previous chapter.
  (Instead of doing the accumulation/aggregation at the end, do it up front with each head)*/

  def foldLeft1[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(xs: List[A], acc: B): B = xs match {
      case Nil => acc
      case Cons(h, t) => go(t, f(acc, h))
    }
    go(as, z)
  }

  /*Exercise 3.11
  Write sum, product, and a function to compute the length of a list using foldLeft.*/
  def sumL[A <% Double /*A <: { def +(x: Double): Double } how would this work instead?*/](as: List[A]): Double =
    foldLeft1(as, 0.0)((d, a) => a + d)

  def productL[A <% Double](as: List[A]): Double =
    foldLeft1(as, 1.0)((d, a) => a * d)

  def lengthL[A](as: List[A]): Int =
    foldLeft1(as, 0)((b, a) => b + 1)

  /*Exercise 3.12
  Write a function that returns the reverse of a list (given List(1,2,3) it returns
  List(3,2,1)). See if you can write it using a fold.*/

  def reverseL[A](as: List[A]): List[A] =
    foldLeft1(as, Nil:List[A])((b, a) => Cons(a, b))

  def reverseR[A](as: List[A]): List[A] = {
    //foldRight(as, Nil:List[A])((a, b) => appendList(b, Cons(a, Nil)))
    foldRight(as, Nil:List[A])((a, b) => append(b, a)) //equivalent
    //foldRight(as, Nil:List[A])((a, b) => foldRight(b, Cons(a, Nil))(Cons(_, _)))
  }

  /*Exercise 3.13
  Hard: Can you write foldLeft in terms of foldRight? How about the other way
  around? Implementing foldRight via foldLeft is useful because it lets us implement
  foldRight tail-recursively, which means it works even for large lists without overflow-
  ing the stack.*/

  def foldLeftR[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverseR(as), z)((a, b) => f(b, a))
  }

  def foldRightL[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft1(reverseL(as), z)((b, a) => f(a, b))
  }
  /*Exercise 3.14
  Implement append in terms of either foldLeft or foldRight.*/

  def append[A](as: List[A], a: A): List[A] =
    foldRight(as, Cons(a, Nil))(Cons(_, _))

  def appendL[A](as: List[A], a: A): List[A] =
    reverseL(Cons(a, reverseL(as)))
    //same as foldRightL(as, Cons(a, Nil))(Cons(_, _))

  def appendListR[A](as: List[A], toAppend: List[A]): List[A] =
    foldRight(as, toAppend)(Cons(_, _))
  /* toAppend match {
    case Nil => as
    case Cons(h, t) => appendListR(append(as, h), t)
  }*/

  def appendListL[A](as: List[A], toAppend: List[A]): List[A] =
    reverseL(foldLeft1(toAppend, reverseL(as))((b, a) => Cons(a, b)))
    //2N run time

  /*toAppend match {
    case Nil => as
    case Cons(h, t) => appendListL(appendL(as, h), t)
  }*/ //for this commented out implementation of appendListL:
  //time:
//for List(as1, as2, ..., asN)
// as1.length to append to Nil
  // Nil.length to reverse
  // as1.length + Nil.length to reverse
// as2.length
  // as1 + nil length to reverse
  // as2 + as1 + Nil length to reverse
//...
// asN.length
  // asN-1 + ... + as1 + Nil length
  // asN + ... + as1 + Nil length
//== 2(N * as1.length + (N - 1) * as2.length + ... + asN.length)
// thinking of asI.length as constant for each I,
// ~~ 2(N+0 + N-1+1 + N-2+2 + ...) ~~ 2(N*N/2) ~~ N^2

  /*Exercise 3.15
  Hard: Write a function that concatenates a list of lists into a single list. Its runtime
  should be linear in the total length of all lists. Try to use functions we have already
  defined.*/

  def prependListLeftToRight[A](as: List[A], toPrepend: List[A]): List[A] =
    foldLeft1(toPrepend, as)((b, a) => Cons(a, b))
   /*toPrepend match {
    case Nil => as
    case Cons(h, t) => prependListLeftToRight(Cons(h, as), t)
  }*/

  def concatList[A](listOfAs: List[List[A]]): List[A] = listOfAs match {
    case Nil => Nil
    case Cons(h, t) => reverseL(foldLeft1(t, reverseL(h))((bs, as) => prependListLeftToRight(bs, as)))
  }//this way, you only have to reverse the first list, then all at the end, rather than the whole acc + asI for each I in (a1, ..., aN)
  //time:
//for as1, as2, ..., asN
// as1.length to reverse
// as2 through asN length to prepend each element
// as1 through asN length to reverse
// == 2 * all elements
//alternate using foldLeft1:
    //foldLeft1(listOfAs, Nil:List[A])((acc, as) => appendListL(acc, as))

  /*Exercise 3.16
  Write a function that transforms a list of integers by adding 1 to each element.
  (Reminder: this should be a pure function that returns a new List!)*/

  def addOne(l: List[Int]): List[Int] =
    foldLeft1(l, Nil:List[Int])((is, i) => appendL(is, i+1))

  /*Exercise 3.17
  Write a function that turns each value in a List[Double] into a String. You can use
  the expression d.toString to convert some d: Double to a String.*/

  def doublesToStrings(ds: List[Double]): List[String] =
    foldLeft1(ds, Nil:List[String])((ss, d) => appendL(ss, d.toString))

  /*Exercise 3.18
  Write a function map that generalizes modifying each element in a list while maintain-
  ing the structure of the list.*/
  def map1[A,B](as: List[A])(f: A => B): List[B] =
    //foldLeft1(as, Nil:List[B])((bs, a) => appendL(bs, f(a)))
    reverseL(foldLeft1(as, Nil:List[B])((b, a) => Cons(f(a), b)))

  def addOne2(l: List[Int]): List[Int] =
    map1(l)(_ + 1)

  def doublesToStrings2(ds: List[Double]): List[String] =
    map1(ds)(_.toString)

  /*Exercise 3.19
  Write a function filter that removes elements from a list unless they satisfy a given
  predicate. Use it to remove all odd numbers from a List[Int].*/

  def filter1[A](as: List[A])(f: A => Boolean): List[A] = {
    @annotation.tailrec
    def go(xs: List[A], acc: List[A]): List[A] = xs match {
      case Nil => acc
      case Cons(h, t) if f(h) => go(t, Cons(h, acc))
      case Cons(h, t) => go(t, acc)
    }

    reverseL(go(as, Nil:List[A]))
  }

  def removeOdds(l: List[Int]): List[Int] =
    filter1(l)(_ % 2 == 0)

  /*Exercise 3.20
  Write a function flatMap that works like map except that the function given will return
  a list instead of a single result, and that list should be inserted into the final resulting
  list. For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in
  List(1,1,2,2,3,3).*/

  def flatMap1[A,B](as: List[A])(f: A => List[B]): List[B] =//try foldRightL
    reverseL(foldLeft1(as, Nil:List[B])((bs, a) => prependListLeftToRight(bs, f(a))))
  //foldLeft1(as, Nil:List[B])((bs, a) => appendListL(bs, f(a)))

  /*Exercise 3.21
  Use flatMap to implement filter.*/
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap1(as)(a => if (f(a)) Cons(a, Nil) else Nil)
  }

  /*Exercise 3.22
  Write a function that accepts two lists and constructs a new list by adding correspond-
  ing elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).*/
  def addIntLists(l1: List[Int], l2: List[Int]): List[Int] = {
    def go(x1: List[Int], x2: List[Int], acc: List[Int]): List[Int] = x1 match {
      case Nil => acc
      case Cons(h1, t1) => x2 match {
        case Nil => acc
        case Cons(h2, t2) => go(t1, t2, appendL(acc, h1 + h2))
      }
    }
    go(l1, l2, Nil)
  }
  //or
  /*{
    if (lengthL(x1) <= lengthL(x2)) {
      flatMap1(x1)()
    }
  }*/

  /*Exercise 3.23
  Generalize the function you just wrote so that it’s not specific to integers or addition.
  Name your generalized function zipWith.*/

  def zipWith[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] = {
    def go(x1: List[A], x2: List[A], acc: List[B]): List[B] = x1 match {
      case Nil => acc
      case Cons(h1, t1) => x2 match {
        case Nil => acc
        case Cons(h2, t2) => go(t1, t2, appendL(acc, f(h1, h2)))
      }
    }
    go(l1, l2, Nil)
  }

  /*Hard: As an example, implement hasSubsequence for checking whether a List con-
  tains another List as a subsequence. For instance, List(1,2,3,4) would have
  List(1,2), List(2,3), and List(4) as subsequences, among others. You may have
  some difficulty finding a concise purely functional implementation that is also effi-
  cient. That’s okay. Implement the function however comes most naturally. We’ll
  return to this implementation in chapter 5 and hopefully improve on it. Note: Any
  two values x and y can be compared for equality in Scala using the expression x == y.
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean*/

}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /*Exercise 3.25
  Write a function size that counts the number of nodes (leaves and branches) in a tree.*/
  def size1[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => size1(left) + size1(right) + 1
  }

  def size2[A](tree: Tree[A]): Int = {
    def go(t: Tree[A], acc: Int): Int = t match {
      case Leaf(_) => acc + 1
      case Branch(left, right) => go(left, 1) + go(right, 0)
    }
    go(tree, 0)
  }

  /*Exercise 3.26
  Write a function maximum that returns the maximum element in a Tree[Int]. (Note:
  In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
  and y.)*/
  def maximum(tree: Tree[Int]): Int = {
    def go(t: Tree[Int]): Int = t match {
      case Leaf(x) => x
      case Branch(left, right) => go(left) max go(right)
    }
    go(tree)
  }

  def maximum2(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(left, right) => maximum2(left) max maximum2(right)
  }

  def maximum3(tree: Tree[Int]): Int = {
    def go(t: Tree[Int], y: Int): Int = t match {
      case Leaf(x) => x max y
      case Branch(left, right) => go(left, go(right, y))
    }
    go(tree, Int.MinValue)
  }

  /*Exercise 3.27
  Write a function depth that returns the maximum path length from the root of a tree
  to any leaf.*/

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => depth(left) max depth(right) + 1
  }

  /*Exercise 3.28
  Write a function map, analogous to the method of the same name on List, that modi-
  fies each element in a tree with a given function.*/

  def mapTree[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(mapTree(left)(f), mapTree(right)(f))
  }

  /*Exercise 3.29
  Generalize size, maximum, depth, and map, writing a new function fold that abstracts
  over their similarities. Reimplement them in terms of this more general function. Can
  you draw an analogy between this fold function and the left and right folds for List?*/

  def foldTree[A,B](tree: Tree[A], z: B)(f: (B, A) => B): B = {
    def go(t: Tree[A], acc: B): B = t match {
      case Leaf(a) => f(acc, a)
      case Branch(left, right) => go(left, go(right, acc))
    }
    go(tree, z)
  }

  def foldTree[A,B](tree: Tree[A])(lf: A => B)(bf: (B, B) => B): B = tree match {
    case Leaf(a) => lf(a)
    case Branch(l, r) => bf(foldTree(l)(lf)(bf), foldTree(r)(lf)(bf))
  }

  def size[A](tree: Tree[A]): Int =
    foldTree(tree)(n => 1)((x, y) => x + y + 1)
}


sealed trait Option[+A] {
  //4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }//is this the same as this.flatMap(a => Some(f(a)))?

  def flatMap[B](f: A => Option[B]): Option[B] =
  //   this match {
  //   case Some(a) => f(a)
  //   case None => None
  // }
  this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    //returns the first option, if defined; else the second
    this.map(Some(_)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    this.flatMap{ a =>
      if (f(a)) Some(a) else None
    }
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  //4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    //If the mean of a sequence is m,
    //the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
    mean(xs) flatMap { m =>
      mean(xs map { x =>
        math.pow(x - m, 2)})
    }
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  //4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(x => b.map(y => f(x, y)))
  }

  import rvolkert.fp.List.foldRightL
  //4.4
  /*Write a function sequence that combines a list of Options into one Option containing
  a list of all the Some values in the original list. If the original list contains None even
  once, the result of the function should be None; otherwise the result should be Some
  with a list of all the values. Here is its signature:3*/
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    foldRightL(a, Some(List[A]()): Option[List[A]]) {
      // (x: Option[A], y: Option[List[A]]) => map2(x, y)(Cons(_, _))
      (x, y) => map2(x, y)(Cons(_, _))
    }
  }

  //4.5 write it so that we only pass through the list once
  //then implement sequence using traverse
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    foldRightL(a, Some(List[B]()): Option[List[B]]) {
      (x, y) => map2(f(x), y)(Cons(_, _))
    }
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(identity)
  }
}


sealed trait Either[+E, +A] {
  //4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def getOrElse[B >: A](b: => B): B = this match {
    case Right(a) => a
    case Left(e) => b
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this.map(Right(_)).getOrElse(b)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this.flatMap(x => b.map(y => f(x, y)))
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  import rvolkert.fp.List.foldRightL
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  //4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    foldRightL(es, Right(List()): Either[E, List[A]]) { (opA, opListA) =>
      opA.map2(opListA)(Cons(_, _))
    }
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    foldRightL(as, Right(List()): Either[E, List[B]]) { (a, opListB) =>
      f(a).map2(opListB)(Cons(_, _))
    }
  }

  def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(identity)
  }
}

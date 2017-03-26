package rvolkert.bookclub

object Exercises {
  def getExerciseLabels(chapter: Int, n: Int) = {
    @annotation.tailrec
    def getExercises(n: Int, acc: List[String]): List[String] = {
      if (n<=0) acc
      else getExercises(n-1, Cons(s"  //Exercise $chapter.$n", acc))
    }
    // getExercises(n, Nil) foreach println
  }

  //Chapter 2
  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, n*acc)
    }
    go(n, 1)
  }

  //Exercise 2.1
  //first try
  def fib1(n: Int): Int = {
    def go(n: Int): Int = {
      if (n<=1) 0
      else if (n==2) 1
      else go(n-1) + go(n-2)
    }
    go(n)
  }
  //tailrec this time
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(timesLeft: Int, f1: Int, f2: Int): Int = {
      if (timesLeft<=1) f1
      else go(timesLeft-1, f2, f1+f2)
    }
    go(n, 0, 1)
  }

  //Exercise 2.2 - using parametric polymorphism with higher-order functions
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.size - 1) true
      else if (!(ordered(as(n), as(n+1)))) false
      else loop(n + 1)
    }
    loop(0)
  }

  def partial1[A,B,C](a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a, b)
  }

  //Exercise 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a,b)
  }

  //Exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  //Exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  //Exercise 3.1
  //3, because x + y = 1 + 2 = 3

  sealed trait List[+A] //covariant, so we can use List[Nothing] as a subtype
                        //of all List[A] and don't have to say Nil[A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = //variadic notation, where A* is syntactic sugar for passing a Seq literal
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

  //Exercise 3.2
    def tail[A](as: List[A]): List[A] = as match {
      case Cons(h, t) => t
      case Nil => Nil //could also define .tail for Nil in the case class and put an abstract def in List trait
                    //or could throw an Exception, as Nil doesn't really have a tail
                    //or could limit the function to only take Cons, rather than all Lists
    }

  //Exercise 3.3
    def setHead[A](as: List[A], newHead: A): List[A] = as match {
      case Cons(h, t) => Cons(newHead, t)
      case Nil => Nil
    }

  //Exercise 3.4
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Cons(h, t) if n > 0 => drop(t, n-1)
      case _ => l
    }

    def tail2[A](l: List[A]): List[A] = drop(l, 1)

  //Exercise 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
      @annotation.tailrec
      def loop(l: List[A]): List[A] = l match {
        case Cons(h, t) if f(h) => loop(t)
        case _ => l
      }
      loop(l)
    }

  //Exercise 3.6
  //return all but the last element
  //this can't be constant time because it's singly linked in the opposite direction
  //we have to dig to the end of the list to find and copy all the values we want
    def init[A](l: List[A]): List[A] = l match {
      case Cons(h, t@Cons(t1, t2)) => Cons(h, init(t))
      case _ => Nil
    }


    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  //Exercise 3.7
  /*Can product, implemented using foldRight, immediately halt the recursion and
  return 0.0 if it encounters a 0.0? Why or why not?
  No. It traces down to the Nil before being able to collapse the evaluation of f on the call stack.

  Consider how any short-circuiting
  might work if you call foldRight with a large list. This is a deeper question that we’ll
  return to in chapter 5.
  It would be nice to short circuit so we didn't have to traverse the whole list. You could add another case for it, though you'd have to limit foldRight to one type, A, or provide another parameter to have a zLike: A
  */

  //Exercise 3.8
  /*See what happens when you pass Nil and Cons themselves to foldRight, like this:
  foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).
  You get the same list back.
  What do you think this says about the relationship between foldRight and the data constructors of List (Nil and Cons)?
  Cons is the idetity function of foldRight, and Nil is its identity element.*/

  //Exercise 3.9
  def lengthOfList[A](as: List[A]): Int = {
    foldRight(as, 0)((x,y) => 1 + y)
  }

  //Exercise 3.10
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    def loop(as: List[A], acc: B): B = as match {
      case Nil => acc
      case Cons(x, xs) => loop(xs, f(acc, x))
    }
    loop(as, z)
  }

  //Exercise 3.11
  def sumL[A <% Double](as: List[A]): Double = {
    foldLeft(as, 0.0)(_ + _)
  }
  def productL[A <% Double](as: List[A]): Double = {
    foldLeft(as, 1.0)(_ * _)
  }
  def lengthL[A](as: List[A]): Int = {
    foldLeft(as, 0)((b,a) => 1 + b)
  }

  //Exercise 3.12
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((b,a) => Cons(a,b))
  }

  //to be used in reverseR
  def append[A](as: List[A], a: A): List[A] = as match {
    case Nil => Cons(a, Nil)
    case Cons(x,xs) => Cons(x, append(xs, a))
  }
  def reverseR[A](as: List[A]): List[A] = {
    foldRight(as, List[A]())((x, xs) => append(xs, x))
  }

  //Exercise 3.13
  def foldLeftR[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverseR(as), z)((a,b) => f(b,a))
  }

  def foldRightL[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b,a) => f(a,b))
  }

  //Exercise 3.14 append in terms of foldLeft or foldRight
  def appendL[A](as: List[A], a: A): List[A] = {
    foldLeft(reverse(as), Cons(a, Nil))((b,a) => Cons(a,b))
  }

  def appendR[A](as: List[A], a: A): List[A] = {
    foldRight(as, Cons(a, Nil))((a,b) => Cons(a,b))
  }

  //Exercise 3.15
  def concatList[A](listOfLists: List[List[A]]): List[A] = listOfLists match {
    case Nil => Nil
    case Cons(h, Nil) => h
    case Cons(h1, Cons(h2, t)) => concatList(Cons(combineLists(h1, h2), t))
  }
  def combineLists[A](xs: List[A], ys: List[A]): List[A] = {
    foldRightL(xs, ys)(Cons(_, _))
  }

  //Exercise 3.16
  def addOne(xs: List[Int]): List[Int] = {
    // def loop(xs: List[Int], acc: List[Int]): List[Int] = xs match {
    //   case Nil => acc
    //   case Cons(h,t) => loop(t, Cons((h+1), acc))
    // }
    // reverse(loop(xs, Nil))
    foldLeft(xs, List[Int]())((ys, x) => appendL(ys, x + 1))
  }

  //Exercise 3.17
  def doublesToStrings(ds: List[Double]): List[String] = {
    foldLeft(ds, List[String]())((ss, d) => appendL(ss, d.toString))
  }

  //Exercise 3.18
  def map1[A,B](as: List[A])(f: A => B): List[B] = {
    foldLeft(as, List[B]())((bs, a) => appendL(bs, f(a)))
  }

  //Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldLeft(as, List[A]())((ms, a) => if (f(a)) appendL(ms, a) else ms)
  }

  def removeOdds(xs: List[Int]): List[Int] = {
    filter(xs)(_ % 2 == 0)
  }

  //Exercise 3.20
  def appendList[A](front: List[A], back: List[A]) = {
    foldLeft(reverse(front), back)((as, a) => Cons(a, as))
  }
  def flatMap1[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldLeft(as, List[B]())((bs, a) => appendList(bs, f(a)))
    // foldRightL(as, List[B]())((a, bs) => appendList(f(a), bs))
  }

  //Exercise 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap1(as)(a => if (f(a)) List(a) else Nil)
  }
  //Exercise 3.22
  def addIntLists(first: List[Int], second: List[Int]): List[Int] = {
    def loop(first: List[Int], second: List[Int], acc: List[Int]): List[Int] = {
      (first, second) match {
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(h, t), Cons(h2, t2)) => loop(t, t2, appendL(acc, h + h2))
      }
    }
    loop(first, second, Nil)
  }

  //Exercise 3.23
  def zipWith[A,B](first: List[A], second: List[A])(f: (A, A) => B): List[B] = {
    def loop(first: List[A], second: List[A], acc: List[B]): List[B] = {
      (first, second) match {
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(h, t), Cons(h2, t2)) => loop(t, t2, appendL(acc, f(h, h2)))
      }
    }
    loop(first, second, List[B]())
  }

  //Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(h,t), Cons(h2,t2)) if h == h2 => startsWith(t,t2) || hasSubsequence(t, sub)
    case (_, Cons(toMatch, _)) => hasSubsequence(dropWhile(sup, (a: A) => a != toMatch), sub)
  }
  //created for use in 3.24
  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Cons(h,t), Cons(h2,t2)) if h == h2 => startsWith(t,t2)
    case _ => false
  }
  }

  sealed trait Tree[+A] //ADT with sum (union) of 2
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    //Exercise 3.25 - count the nodes
    def treeSize[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l,r) => 1 + treeSize(l) + treeSize(r)
    }

    def treeSizeLoop[A](tree: Tree[A]): Int = {
      def loop(t: Tree[A], acc: Int): Int = t match {
        case Leaf(_) => acc + 1
        case Branch(l,r) => loop(l, loop(r, acc + 1))//couldn't model this acc + 1 with oldFold - needed lf and bf
      }
      loop(tree, 0)
    }

    // Exercise 3.26
    def maximum(tree: Tree[Int]): Int = {
      def loop(t: Tree[Int], acc: Int): Int = t match {
        case Leaf(x) => acc max x
        case Branch(l,r) => loop(l, loop(r, acc))
      }
      loop(tree, Int.MinValue)
    }

    def maximum2(tree: Tree[Int]): Int = tree match {
      case Leaf(x) => x
      case Branch(l,r) => maximum2(l) max maximum2(r)
    }

    //Exercise 3.27
    //max path length to a leaf
    def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 0
      case Branch(l,r) => 1 + (depth(l) max depth(r))
    }

    //Exercise 3.28
    def mapTree[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l,r) => Branch(mapTree(l)(f), mapTree(r)(f))
    }

    //Exercise 3.29
    def fold[A,B](tree: Tree[A])(lf: A => B)(bf: (B, B) => B): B = tree match {
      case Leaf(a) => lf(a)
      case Branch(l,r) => bf(fold(l)(lf)(bf), fold(r)(lf)(bf))
    }

    //didn't work because it didn't have a way to combine branches into a new B
    //could only combine acc with leaves
    def oldFold[A,B](tree: Tree[A], z: B)(f: (A, B) => B): B = {
      def loop(t: Tree[A], acc: B): B = t match {
        case Leaf(a) => f(a,acc)
        case Branch(l,r) => loop(l, loop(r, acc))
      }
      loop(tree, z)
    }

    def sizeFold[A](t: Tree[A]): Int = {
      fold(t)(_ => 1)((lVal,rVal) => 1 + lVal + rVal)
    }
    def maximumFold(t: Tree[Int]): Int = {
      fold(t)(identity)((l,r) => l max r)
    }
    def depthFold[A](t: Tree[A]): Int = {
      fold(t)(_ => 0)((l,r) => 1 + (l max r))
    }
    def mapFold[A,B](t: Tree[A])(f: A => B): Tree[B] = {
      fold(t)(a => Leaf(f(a)):Tree[B])((l,r) => Branch(l, r))
    }
  }
}

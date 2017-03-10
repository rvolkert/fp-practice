package rvolkert.bookclub

object Exercises {
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

}

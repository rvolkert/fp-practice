package rvolkert.bookclub

object Chapter7 {

  type Par[A] // Par for parallel
  object Par {
    //takes a value and evaluates it on another logical thread
    //begins evaluating as soon as it's referenced, since scala's left-to-right evaluation
    //would force it to be sequential if we waited until get was called on it
    def unit[A](a: => A): Par[A] // does this need to be lazy, now that we have map2? 7.1

    //avoid using get besides at the edge of the program, since it needs to
    def get[A](a: Par[A]): A

    // Ex 7.1: write the signature of map2
    def map2[A,B](parA: => Par[A], parB: => Par[B])(f: (A, B) -> C): Par[C]
    //7.1 we conclude that because of l to r eval, we need these to be lazy so we aren't forced
    //to unpack the l side before getting to the r
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) {
      Par.unit(ints.headOption getOrElse 0)
    }
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
}

package rvolkert.bookclub

import Chapter4._
import Chapter4.Option._
import rvolkert.bookclub.Exercises.{List, Nil, Cons}
import rvolkert.bookclub.Exercises.List._
import org.specs2.mutable.Specification

object Chapter4Spec extends Specification {
  "Options" should {
    "map" in {
      Some(1).map(_ + 1) === Some(2)
    }
    "flatMap" in {
      Some(1).flatMap(Some(_)) === Some(1)
      None.flatMap(Some(_)) === None
    }
    "getOrElse" in {
      Some(1).getOrElse(5) === 1
      None.getOrElse(5) === 5
    }
    "orElse" in {
      Some(1).orElse(Some(2)) === Some(1)
      None.orElse(Some(2)) === Some(2)
    }
    "filter" in {
      Some(1).filter(_ == 1) === Some(1)
      Some(2).filter(_ == 1) === None
    }
    "work for mean" in {
      mean(Seq(1,2,4,5)) === Some(3)
    }
    "work for variance" in {
      variance(Seq(1,2,4,5)) === Some(2.5)
    }
    "map2" in {
      map2(Some("a"), Some("b"))(_ + _) === Some("ab")
      map2(Some("a"), None)(_ + _) === None
      map2(None, Some("b"))((s1: String, s2: String) => s1 + s2) === None
    }
    "sequence" in {
      Option.sequence(List(Some(1), Some(2), Some(3))) === Some(List(1,2,3))
      Option.sequence(List(Some(1), None, Some(3))) === None
    }
    "traverse" in {
      Option.traverse(List(1,2,3))(Some(_)) === Some(List(1,2,3))
      def evenToNone(x: Int): Option[Int] = if (x % 2 == 0) None else Some(x)
      Option.traverse(List(1,2,3))(evenToNone) === None
    }
    "sequence2" in {
      Option.sequence2(List(Some(1), Some(2), Some(3))) === Some(List(1,2,3))
      Option.sequence2(List(Some(1), None, Some(3))) === None
    }
  }

  "Eithers" should {
    "map" in {
      Right(1).map(_ + 5) === Right(6)
      (Left("wrong"): Either[String, Int]).map(_ + 5) === Left("wrong")
    }
    "flatMap" in {
      Right(1).flatMap((i: Int) => Right(i + 5)) === Right(6)
      (Left("wrong"): Either[String, Int]).flatMap((i: Int) => Right(i + 5)) === Left("wrong")
    }
    "getOrElse" in {
      Right(1).getOrElse(_ => 2) === 1
      Left("wrong").getOrElse(_ => 2) === 2
    }
    "orElse" in {
      Right(1).orElse(Right(2)) === Right(1)
      Left("wrong").orElse(Right(2)) === Right(2)
    }
    "map2" in {
      Right(5).map2(Right(2))(_ * _) === Right(10)
      (Left("wrong"): Either[String, Int]).map2(Right(2))(_ * _) === Left("wrong")
      Right(3).map2(Left("wrong"))((x: Int, y: Int) => x * y) === Left("wrong")
    }
    "sequence" in {
      Either.sequence(List(Right(1), Right(2), Right(3))) === Right(List(1,2,3))
      Either.sequence(List(Right(1), Left("hi"), Left("bye"), Right(4))) === Left("hi")
    }
    "traverse" in {
      Either.traverse(List(1,2,3))(Right(_)) === Right(List(1,2,3))
      def oddToLeft(x: Int): Either[String, Int] = if (x % 2 != 0) Left(s"$x is odd") else Right(x)
      Either.traverse(List(1,2,3))(oddToLeft) === Left("1 is odd")
    }
    "sequence" in {
      Either.sequence2(List(Right(1), Right(2), Right(3))) === Right(List(1,2,3))
      Either.sequence2(List(Right(1), Left("hi"), Left("bye"), Right(4))) === Left("hi")
    }
  }
}

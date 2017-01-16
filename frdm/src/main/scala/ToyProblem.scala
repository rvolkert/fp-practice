import scalaz.Kleisli
import Kleisli._
import scalaz.std.option._ //needed for the Option monad, to have the bind for andThen

// A => M[B]
// Repository => M[A]

trait Repository

trait PrisonSystem[ID, Prisoner, Sentence] {
  def getPrisoner(id: ID): Kleisli[Option, Repository, Prisoner]
  def getSentences: Kleisli[Option, Prisoner, Sentence]
  def getYearOfSentence: Kleisli[Option, Sentence, Int]

  def getYears(id: ID): Kleisli[Option, Repository, Int] = {
    getPrisoner(id) andThen getSentences andThen getYearOfSentence
  }
}

case class ID(i: Int) //unique to each prisoner
case class Prisoner(name: String, i: ID)
case class Sentence(offense: String, years: Int)

trait PrisonSystem2[ID, Prisoner, Sentence] {
  import scalaz.NonEmptyList
  import NonEmptyList._
  import scalaz.\/
  import \/._
  import frdm.exercises.Exercises._
  import frdm.exercises.Exercises.listFold
  type Valid[A] = \/[NonEmptyList[String], A]

  def getPrisoner(id: ID): Kleisli[Valid, Repository, Prisoner]
  def getSentences(p: Prisoner): Kleisli[Valid, Repository, List[Sentence]]
  def getYearOfSentence(s: Sentence): Int

  def getYears(id: ID): Kleisli[Valid, Repository, Int] = {
    val F = implicitly[Foldable[List]]
    for {
      prisoner <- getPrisoner(id) // completely un-nests the Prisoner
      sentences <- getSentences(prisoner) // looks like we're not passing anything,
                        // but think of it as a flatmap of getPrisoner onto getSentences
    } yield {
      // sentences.foldLeft(0)((y, s) => y + getYearOfSentence(s)) // could do this, or use our intAdditionMonoid and listFold from Exercises
      F.foldMap(sentences)(getYearOfSentence)(intAdditionMonoid)
    }
  }
}

// object PrisonSystem extends PrisonSystem[ID, Prisoner, Sentence] {
//
// }

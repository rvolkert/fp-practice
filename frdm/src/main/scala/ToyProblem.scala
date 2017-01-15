import scalaz.Kleisli
import Kleisli._
import scalaz.std.option._ //needed for the Option monad, to have the bind for andThen

// A => M[B]
// Repository => M[A]

trait Repository

trait PrisonSystem[ID, Prisoner, Sentence] {
  def getPrisoner(id: ID): Kleisli[Option, Repository, Prisoner]
  def getSentences: Kleisli[Option, Prisoner, Sentence]
  def getYearOfSentence(s: Sentence): Int

  def getYears(id: ID): Kleisli[Option, Repository, Sentence] = {
    getPrisoner(id) andThen getSentences
    // Previous attempt, when we had getSentences take a Prisoner and return Kleisli[Option, Repository, Sentence]
    // for {
    //   p <- getPrisoner(id) // completely un-nests the Prisoner
    //   s <- getSentences // looks like we're not passing anything,
    //                     // but think of it as a flatmap of getPrisoner onto getSentences
    // } yield {
    //   getYearOfSentence(s)
    // }
  }
}

case class ID(i: Int)
case class Prisoner(name: String, i: ID)
case class Sentence(offense: String, years: Int)

// object PrisonSystem extends PrisonSystem[ID, Prisoner, Sentence] {
//
// }

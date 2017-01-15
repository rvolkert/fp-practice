package frdm.algebras

import scala.util.Try
import java.util.Date

trait AccountService[Account, Amount, Balance] {
  def open(no: String, name: String, openDate: Try[Date]): Try[Account]
  def close(account: Account, closeDate: Try[Date]): Try[Account]
  def debit(account: Account, amount: Amount): Try[Account]
  def credit(account: Account, amount: Amount): Try[Account]
  def balance(account: Account): Try[Balance]

  def transfer(from: Account, to: Account, amount: Amount): Try[(Account, Account, Amount)] = for {
    a <- debit(from, amount)
    b <- credit(to, amount)
  } yield (a, b, amount)
}

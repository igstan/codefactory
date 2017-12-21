package codefactory

import cats.{ Applicative, Monad }
import cats.data.Validated.{ Invalid, Valid }
import cats.implicits._

object Validations {
  case class Country(name: String) extends AnyVal
  case class User(age: Int, country: Country)

  def userAge: Either[String, Int] =
    Right(33)

  def userCountry: Either[String, Country] =
    Left("service not available in your country")

  //def user1[F[_]](userAge: F[Int], userCountry: F[Country])(implicit FM: Monad[F]): F[User] =
  def user1[F[_]: Monad](userAge: F[Int], userCountry: F[Country]): F[User] =
    for {
      age     <- userAge
      country <- userCountry
    } yield {
      User(age, country)
    }

  def user2[F[_]: Applicative](userAge: F[Int], userCountry: F[Country]): F[User] =
    (userAge, userCountry).mapN { (age: Int, country: Country) =>
      User(age, country)
    }

  def main(args: Array[String]): Unit = {
    val validationReport = user2(
      userAge.toValidatedNel,
      userCountry.toValidatedNel
    )

    validationReport match {
      case Valid(user) => println(s"user: $user")
      case Invalid(errors) =>
        println(s"errors: ${errors.toList.mkString("\n- ", "\n- ", "")}")
    }
  }
}

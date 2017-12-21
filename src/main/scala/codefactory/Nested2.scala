package codefactory

import cats.{ Applicative, Monad }
import cats.data.{ Nested, ValidatedNel }
import cats.implicits._
import monix.eval.Task

object Nested2 {
  case class Country(name: String) extends AnyVal
  case class User(age: Int, country: Country)

  def userAge: Task[Either[String, Int]] =
    Task.now(Right(33))
    // Task.now ~= Future.successful

  def userCountry: Task[Either[String, Country]] =
    Task.now(Left("service not available in your country"))

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

  // F[G[A]] => H[A]

  def main(args: Array[String]): Unit = {
    val validationReport = user2(
      // Task[ValidatedNel[String, Int]] => Task[ValidatedNel[String, ?]]
      Nested[Task, ValidatedNel[String, ?], Int](
        userAge.map(_.toValidatedNel)
      ),
      // type lambdas == type-level anonymous functions
      Nested[Task, ValidatedNel[String, ?], Country](
        userCountry.map(_.toValidatedNel)
      )
    )

    //validationReport match {
    //  case Valid(user) => println(s"user: $user")
    //  case Invalid(errors) =>
    //    println(s"errors: ${errors.toList.mkString("\n- ", "\n- ", "")}")
    //}
  }
}

package codefactory

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import cats.data.{ EitherT, OptionT }
import cats.implicits._

object MonadTransformers {
  def f1: Future[Option[String]] =
    Future.successful(None)

  def f2(a: String): Future[Option[Int]] =
    Future.successful {
      Try(a.toInt).toOption
    }

  def composed1: Future[Option[Int]] =
    for {
      a <- f1
      b <- a match {
        case None => Future.successful(None)
        case Some(a) => f2(a)
      }
    } yield {
      b
    }

  def composed2 =
    for {
      a <- OptionT(f1)
      b <- OptionT(f2(a))
    } yield {
      b
    }

  def composed3: EitherT[Future, String, Int] =
    for {
      a <- OptionT(f1).toRight(left = "missing f1 value")
      b <- OptionT(f2(a)).toRight(left = "couldn't parse as int")
    } yield {
      b
    }

  def main(args: Array[String]): Unit = {
    val future = composed3.value
    val result = Await.result(future, Duration.Inf)

    println(s"result: $result")
  }
}

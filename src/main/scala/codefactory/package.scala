import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.duration.FiniteDuration
import scala.util.{ Failure, Success }
import com.typesafe.scalalogging.Logger
import monix.eval.Task

package object codefactory {
  implicit final class FutureOps[+A](val future: Future[A]) extends AnyVal {
    def logFailure(message: => String = null)(implicit log: Logger, ec: ExecutionContext): Future[A] = {
      future.transform {
        case success @ Success(_) => success
        case failure @ Failure(t) =>
          log.error(if (message ne null) message else t.getMessage, t)
          failure
      }
    }
  }

  implicit final class TaskOps[A](val task: Task[A]) extends AnyVal {
    def logFailure(msg: String)(implicit log: Logger): Task[A] =
      task.doOnFinish {
        case Some(throwable) => Task.now(log.error(msg, throwable))
        case None => Task.unit
      }

    def onErrorRetryEvery(duration: FiniteDuration): Task[A] = {
      def retry(e: Throwable): Task[A] =
        task.onErrorHandleWith(retry).delayExecution(duration)

      task.onErrorHandleWith(retry)
    }
  }
}

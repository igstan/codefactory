package codefactory

import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.duration._
import scala.util.Try
import monix.eval.Task

object Tasks {
  def future: Future[Int] = {
    import monix.execution.Scheduler.Implicits.global

    val ff = Future.failed(new RuntimeException("Booom!"))
    val fs = Future.successful(1)

    val fa = Future.apply( println("foo") )

    val task = Task {
      println("task running")
      1
    }

    val result =
      task
        .onErrorRetryEvery(10.seconds)
        .runAsync

    result
  }

  def task: Task[Int] =
    Task.deferFutureAction { implicit scheduler =>
      future
    }

  def listFuture(futures: List[Future[String]])(implicit ec: ExecutionContext): Future[List[Try[String]]] = {
    val tries: List[Future[Try[String]]] =
      futures.map { future =>
        future.transformWith(`try` => Future.successful(`try`))
      }

    Future.sequence(tries)
  }

  def listTask(tasks: List[Task[String]]): Task[List[String]] = {
    val sequential: Task[List[Either[Throwable, String]]] =
      Task.sequence(tasks.map(_.attempt))
    val parallel = Task.gather(tasks)

    ???
  }
}

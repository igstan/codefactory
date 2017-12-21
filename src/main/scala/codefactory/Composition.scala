package codefactory

import cats.{ Applicative, Functor }

object Composition {
  def composeFunctions[A, B, C](f: A => B, g: B => C): A => C =
    a => g(f(a))
    // g.compose(f)
    // f.andThen(g)

  def composeFunctions2[A, B, C](f: Function[A, B], g: Function[B, C]): Function[A, C] =
    a => g(f(a))
  // g.compose(f)
  // f.andThen(g)

  def composeFunctors[F[_]: Functor, G[_]: Functor, R]
    (fa: F[R], ga: G[R]): Functor[Lambda[A => F[G[A]]]] =
    new Functor[Lambda[A => F[G[A]]]] {
      override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] = {
        Functor[F].map(fa) { ga =>
          Functor[G].map(ga) { a =>
            f(a)
          }
        }
      }
    }

  def composeApplicative[F[_]: Applicative, G[_]: Applicative, R]
    (fa: F[R], ga: G[R]): Applicative[Lambda[A => F[G[A]]]] = {
    // homework
    ???
  }

  // Monads don't compose.
  // def composeMonads[F[_]: Monad, G[_]: Monad, R]
}

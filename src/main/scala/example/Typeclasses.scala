package example

object Typeclasses {
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Applicative[F[_]] extends Functor[F] {
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
    def pure[A](a: A): F[A]
  }

  trait Monad[F[_]] extends Applicative[F] {
    def pure[A](value: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
  }
}

package example

object Typeclasses {
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Monad[F[_]] {
    def pure[A](value: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
  }
}

package example

import cats.data.Kleisli
import cats.implicits._

object SafeGetters {

  // A nested Java object structure with nullable fields
  class Token(val content: String)
  class APIClientConfig(val host: String, val version: Int, val token: Token)
  class JavaAPIClient(val config: APIClientConfig)

  val bad: JavaAPIClient = new JavaAPIClient(new APIClientConfig("host", 1, null))
  val good: JavaAPIClient = new JavaAPIClient(new APIClientConfig("host", 1, new Token("content")))

  //scala> bad.config.token.content.length
  //java.lang.NullPointerException
  //  ... 36 elided


  // If we lift the getter into Option, we get an arrow from A => F[B]
  // This is known as a Kleisli arrow
  type SafeGetter[A, B] = Kleisli[Option, A, B]
  def safeGetter[A, B](f: A => B): Kleisli[Option, A, B] = Kleisli(a => Option(f(a)))

  implicit class SafeGetterOps[A, B, C](sg: SafeGetter[A, B]) {
    private def lift(f: B => C): B => Option[C] = b => Option(f(b))
    def ?(f: B => C): SafeGetter[A, C] = sg andThen lift(f)
  }

  // Compose our Option-lifted getters to make a safe accesor function
  val tokenLength: SafeGetter[JavaAPIClient, Int] =
    safeGetter[JavaAPIClient, APIClientConfig](_.config) ? (_.token) ? (_.content) ? (_.length)
}

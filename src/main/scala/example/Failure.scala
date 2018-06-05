import cats.Applicative
import cats.Applicative._

import scala.util.Try
import cats.data._
import cats.implicits._

object Failure {

  /*
   * Modeling a website signup flow.
   */
  def signupFlow(request: AccountRequest): AccountResponse = ???

  sealed case class AccountRequest(
    user: String, pw: String, email: String)
  sealed trait AccountResponse
  case class Denied(msg: String) extends AccountResponse
  case class Granted(
    user: String, encryptedPW: String, validEmail: Boolean)
    extends AccountResponse

  // But our `encrypt` function has an unchecked exception
  def encrypt(text: String, seed: Int): String =
    if (text.contains("mailman")) {
      throw new IllegalArgumentException("Bad input!")
    } else {
      text.map(i => (i << 1).toChar)
    }

  // Unsafe call to `head` can throw NoSuchElementError
  def validateEmail(email: String): Boolean = {
    val validDomains = Set("com", "org", "gov")
    validDomains contains email.split('.').tail.head
  }

  // The "Java" style looks simple here, but it isn't composable.
  def signupFlowImperative(request: AccountRequest): AccountResponse = {
    try {
      val encrypted = encrypt(request.pw, 10)
      val validEmail = validateEmail(request.email)
      Granted(request.user, encrypted, validEmail)
    } catch {
      case e: Throwable => Denied(e.getMessage)
    }
  }

  // Use Try if we want to use the offending function as-is
  def signupFlowFunctional(request: AccountRequest): AccountResponse =
    Try(encrypt(request.pw, 10))
      .flatMap(encrypted =>
        Try(validateEmail(request.email))
          .map(valid => Granted(request.user, encrypted, valid)))
      .fold(t => Denied(t.getMessage), g => g)

  // For comprehension cleans up the nesting
  def signupFlowFunctional2(request: AccountRequest): AccountResponse = {
    val tryGrant = for {
      encrypted  <- Try(encrypt(request.pw, 10))
      validEmail <- Try(validateEmail(request.email))
    } yield Granted(request.user, encrypted, validEmail)

    tryGrant.fold(t => Denied(t.getMessage), g => g)
  }

  // Because the two `Try` not depend on each other, we can use a weaker combinator...
  def signupFlowFunctional3(request: AccountRequest): AccountResponse =
    Applicative[Try].map2(
      Try(encrypt(request.pw, 10)), Try(validateEmail(request.email)))(
        (encrypted, validEmail) => Granted(request.user, encrypted, validEmail)
    ).fold(t => Denied(t.getMessage), g => g)


  // Example 2: We implement the functions ourselves or just wrap them
  def wrappedEncrypt(text: String, seed: Int): Either[Throwable, String] =
    Either.catchNonFatal(encrypt(text, seed))


//  def wrapped(request: AccountRequest): AccountResponse =


//  sealed trait Either[+A, Throwable]
//  case class Left[A](t: Throwable) extends Either[A, Throwable]
//  case class Right[A](a: A) extends Either[A, Throwable]
//
//  sealed trait Try[+A]
//  case class Failure(t: Throwable) extends Try[Throwable]
//  case class Success[A](a: A) extends Try[A]

}


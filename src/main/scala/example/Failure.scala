package example

import cats.Applicative

import scala.util.Try
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
    user: String, encryptedPW: String, validEmail: Boolean
  ) extends AccountResponse

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

  /*
   * Because the two `Try`s don't depend on each other
   * we can use a weaker combinator...
   * Look familiar? Closer to the imperative example.
   */
  def signupFlowFunctional3(request: AccountRequest): AccountResponse = {
    val encrypted = Try(encrypt(request.pw, 10))
    val validated = Try(validateEmail(request.email))
    Applicative[Try].map2(encrypted, validated)(
      (e, v) => Granted(request.user, e, v)
    ).fold(t => Denied(t.getMessage), g => g)
  }


  // We can use a type alias to bind Either's left side as Throwable
  type Result[A] = Either[Throwable, A]

  def safeEncrypt(text: String, seed: Int): Result[String] =
    Either.catchNonFatal(encrypt(text, seed))

  def safeValidate(email: String): Result[Boolean] =
    Either.catchNonFatal(validateEmail(email))


  // This lets us get the Applicative Functor instance of Either
  def safeSetup(req: AccountRequest): Result[AccountResponse] =
    Applicative[Result]
      .map2(safeEncrypt(req.pw, 1), safeValidate(req.email))(
        (pw, valid) => Granted(req.user, pw, valid)
      )

  def safeSignup(request: AccountRequest): AccountResponse =
    safeSetup(request).fold(t => Denied(t.getMessage), identity)

  //scala> safeSignup(AccountRequest("Newman", "mailman1999", "newman@usps.gov"))
  //res0: AccountResponse = Denied(Bad input!)

  //scala> safeSignup(AccountRequest("Jerry", "seinfeld", "jerry@comedy.com"))
  //res1: AccountResponse = Granted(Jerry,æÊÒÜÌÊØÈ,true)

}


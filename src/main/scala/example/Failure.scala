import scala.util.Try

object FailureExample {

  // Has an unchecked exception
  def encrypt(text: String, seed: Int): String =
    if (text.contains("festivus")) {
      throw new IllegalArgumentException("Bad input!")
    } else {
      text.map(c => (c << 1).toChar)
    }

  case class AccountRequest(u: String, pw: String)

  sealed trait AccountResponse
  case class Denied(msg: String) extends AccountResponse
  case class Granted(e: String, msg: String) extends AccountResponse

  def welcomeMsg: AccountRequest => String =
    r => s"Welcome ${r.u}!"

  //...

  def loginJava(request: AccountRequest): AccountResponse = {
    var message = ""
    var encrypted = ""
    try {
      encrypted = encrypt(request.pw, 10)
      Granted(encrypted, welcomeMsg(request))
    } catch {
      case t: Throwable => Denied(t.getMessage)
    }
  }

  // Example 1: The function is out of our control.
  def loginScala(request: AccountRequest): AccountResponse =
    Try(encrypt(request.pw, 10))
      .map(e => Granted(e, welcomeMsg(request)))
      .getOrElse(Denied("Bad password!"))


  // Example 2: If we are implementing the function ourselves,
  // we can return Either and fold the result to collapse context
  def wrappedEncrypt(t: String, s: Int): Either[Throwable, String] =
    if (t.contains("festivus"))
      Left(new IllegalArgumentException(
        "Password can't contain the word 'festivus'"))
    else
      Right(encrypt(t, s))

  def loginScala2(request: AccountRequest): AccountResponse =
    wrappedEncrypt(request.pw, 10)
      .fold(t => Denied(t.getMessage),
        s => Granted(s, welcomeMsg(request)))

  // Example 3: A different flow producing a User as well
  case class User(name: String, e: String)

  def nameValidator(n: String): Either[Throwable, String] =
    if (n == "Newman")
      Left(new IllegalArgumentException("Hello... Newman"))
    else Right(n)

  // Functor behavior
  def makeUser(name: String, e: String): Either[Throwable, User] =
    nameValidator(name)
      .right
      .map(e => User(name, e))

  // Monad behavior
  def loginScala3(request: AccountRequest): Option[(Granted, User)] = {
    wrappedEncrypt(request.pw, 10).right
      .flatMap(e => makeUser(request.u, e)).right
      .map(user => (Granted(user.name, welcomeMsg(request)), user)).right
      .toOption
  }

}

//  // The Java way
//  val result: Int = try {
//    canFail(1, 0)
//  } catch {
//    case e: Exception => 0
//  }
//
//  // The functional way, using Try
//  def canTry(x: Int, y: Int): Try[Int] = Try(canFail(x, y))
//
//  // Transform the result, or propagate exception
//  canTry(1, 0).map(_ + 1) // Failure(java.lang.ArithmeticException: / by zero)
//
//  // Using Either - more verbose but more flexible
//  def canEither(x: Int, y: Int): Either[String, Int] =
//    if (y == 0) Left("Div by zero")
//    else Right(canFail(x, y))
//
//  // Either is foldable
//  canEither(1, 0).fold(println(_), _ + 1)
//
//}

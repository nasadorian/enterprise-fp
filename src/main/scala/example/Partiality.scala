
object Partiality {

  val personLocation = Map(
    "Jerry Seinfeld" -> "Manhattan",
    "George Costanza" -> "Manhattan",
    "Frank Costanza" -> "Queens",
    "Elaine Benes" -> "Brooklyn"
  )

  val locationTransit = Map(
    "Manhattan" -> List("J", "Z"),
    "Brooklyn" -> List("L")
  )

  // Scala's Map implements get
  // def get[K,V](k: K): Option[V]
  // ...

  def listStopsScala(name: String): String =
    personLocation.get(name)
      .flatMap(locationTransit.get)
      .map(_.reduce(_ + ", " + _))
      .getOrElse("No stops or user not found!")


  def listStopsScala2(name: String): String =
    personLocation.get(name)
      .flatMap(locationTransit.get)
      .fold("No stops or user not found!")(_.reduce(_ + ", " + _))


  def listStopsJava(name: String): String = {
    val location = personLocation.get(name)
    var message = ""
    if (location.isDefined) {
      val transit = locationTransit.get(location.get)
      if (transit.isDefined) {
        var output = collection.mutable.StringBuilder.newBuilder
        for (s <- transit.get) {
          output.append(s + ",")
        }
        message = output.result()
      } else {
        message = "No stops or user not found!"
      }
    } else {
      message = "No stops or user not found!"
    }
    return message
  }

  sealed trait Option[+A]
  case object None extends Option[Nothing]
  case class Some[A](a: A) extends Option[A]


  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Monad[F[_]] {
    def pure[A](value: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
  }

}



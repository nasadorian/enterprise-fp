package example

object Partiality {

  // Scala's Map implements get
  // def get[K,V](k: K): Option[V]

  val personLocation = Map(
    "Jerry Seinfeld" -> "Manhattan",
    "George Costanza" -> "Manhattan",
    "Frank Costanza" -> "Queens",
    "Elaine Benes" -> "Brooklyn"
  )

  val locationTransit = Map(
    "Manhattan" -> List("J", "Z"),
    "Brooklyn" -> List("L", "G", "K")
  )

  // 1. Select person from personLocation,
  // 2. Select their subway stops from locationTransit
  // 3. Transform their stops into a comma-separated string
  def listStops(name: String): String = ???

  // Java-style version abusing Option.isDefined
  def listStopsImperative(name: String): String = {
    val location = personLocation.get(name)
    var message: Option[String] = None
    if (location.isDefined) {
      val transit = locationTransit.get(location.get)
      if (transit.isDefined) {
        // Build the output string
        var output = collection.mutable.StringBuilder.newBuilder
        for (s <- transit.get) {
          output.append(s + ",")
        }
        message = Some(output.result())
      }
    }
    if (message.isDefined) message.get
    else "User or stops not found!"
  }

  // Scala version using flatMap and map combinators
  def listStopsFunctional(name: String): String =
    personLocation.get(name)
      .flatMap(locationTransit.get)
      .fold("User or stops not found!")(_.mkString(","))

  // Using the for-comprehension syntax sugar
  def listStopsSugar(name: String): Option[String] =
    for {
      location <- personLocation.get(name)
      transit  <- locationTransit.get(location)
    } yield transit.mkString(",")

  def listStops2(name: String): String = listStopsSugar(name).getOrElse("Not found!")

  //scala> listStopsFunctionalSugar("Jerry Seinfeld")
  //res0: String = J,Z

  //scala> listStopsFunctionalSugar("Newman")
  //res1: String = Not found!

}



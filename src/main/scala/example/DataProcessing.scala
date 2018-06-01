package example

object DataProcessing {

  case class User(name: String, age: Int, location: String)

  val users = List(
    User("Jim", 99, "Palm Beach"),
    User("Mary", 30, "San Francisco"),
    User("Bill", 23, "San Francisco"),
    User("Stevie", 31, "Los Angeles")
  )

  // Oldest user
  users.maxBy(_.age)

  // Map unique locations to names of users that live there
  users
    .groupBy(_.location)
    .mapValues(_.map(_.name))

  // Window contiguous San Francisco users and transform
  users
    .filter(_.location.equals("San Francisco"))
    .sliding(2)
    .map(l => l(0).name + " lives next to " + l(1).name)
    .toList

}



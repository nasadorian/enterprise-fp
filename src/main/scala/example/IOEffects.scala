package example

import cats.effect.IO
import cats.implicits._
import cats.data._

import scala.collection.mutable

object IOEffects {

  // Retrieving Records from data lake (S3)
  sealed trait Record
  case class Location(name: String) extends Record
  case class Coordinates(lat: Double, lng: Double) extends Record
  case class Restaurant(name: String, location: Location) extends Record

  val restaurantsDatalake: mutable.Map[String, Restaurant] = mutable.Map(
    "soup" -> Restaurant("Soup Place", Location("Queens")),
    "kabob" -> Restaurant("Babu Bhatt's", Location("Brooklyn")),
    "diner" -> Restaurant("Tom's Diner", Location("Manhattan"))
  )

  val locationsDatalake: mutable.Map[Location, Coordinates] = mutable.Map(
    Location("Queens") -> Coordinates(40.10, 74.11),
    Location("Brooklyn") -> Coordinates(41.12, 79.00),
    Location("Manhattan") -> Coordinates(44.00, 70.26)
  )

  val reviewsDatalake: mutable.Map[Restaurant, List[String]] = mutable.Map(
    Restaurant("Soup Place", Location("Queens")) -> List("Guy at counter was really rude."),
    Restaurant("Babu Bhatt's", Location("Brooklyn")) -> List("They changed their entire menu!")
  )

  def fetch[K, V](bucket: mutable.Map[K, V], key: K): Option[V] = {
    Thread.sleep(1000)
    bucket.get(key)
  }

  def write[K, V](bucket: mutable.Map[K, V], key: K, value: V): Int = {
    Thread.sleep(1000)
    bucket.put(key, value)
    1
  }

  def searchRestaurantsJava(search: String): (Restaurant, Coordinates) = {
    var restaurant: Restaurant = null
    var coordinates: Coordinates = null
    try {
      restaurant = fetch(restaurantsDatalake, search).get
      if (restaurant != null) {
        coordinates = fetch(locationsDatalake, restaurant.location).get
      }
    } catch {
      case e: Exception => println("Fetch failed somewhere... we don't know")
    }
    (restaurant, coordinates)
  }

  // Example 1: Cats IO, fetch
  // Unsafe means "use this at the end of the world"
  // In pure FP, the effect and the business logic can be decoupled
  // i.e. we have composable programs that deal with values first, and we can run their effects later
  def ioFetch[K,V](dl: mutable.Map[K, V], key: K): OptionT[IO,V] =
    OptionT[IO,V](IO(fetch(dl, key)))

  def ioWrite[K,V](bucket: mutable.Map[K, V], key: K, value: V): IO[Int] =
    IO(write(bucket, key, value))

  // 2 improvements: for comprehension makes it clearer, and `attempt` gives error handling
  def searchRestaurants(search: String): Either[Throwable, Option[(Restaurant, Coordinates)]] = {
    val io: OptionT[IO, (Restaurant, Coordinates)] = for {
      restaurant  <- ioFetch(restaurantsDatalake, search)
      coordinates <- ioFetch(locationsDatalake, restaurant.location)
    } yield (restaurant, coordinates)

    io.value.attempt.unsafeRunSync()
  }

  // Example 2: Cats IO, fetch and write
  // Isolate the query from the last example and reuse it.. composability!
  def selectRestaurant(search: String): OptionT[IO, (Restaurant, Coordinates)] =
    for {
      restaurant  <- ioFetch(restaurantsDatalake, search)
      coordinates <- ioFetch(locationsDatalake, restaurant.location)
    } yield (restaurant, coordinates)


  // An insert query to write a review
  def insertReview(restaurant: Restaurant, review: String): IO[Int] =
    for {
      reviews     <- ioFetch(reviewsDatalake, restaurant).value
      insert      =  reviews.fold(List(review))(review :: _)
      numInserted <- ioWrite(reviewsDatalake, restaurant, insert)
    } yield numInserted


  //  Bring it all together to search a restaurant then review it
  def lazyReviewer(search: String, review: String): Either[Throwable, Option[Int]] = {
    val io: OptionT[IO, Int] = for {
      (restaurant, location) <- selectRestaurant(search)
      reviewsInserted <-
        if (location.lat > 40)
          insertReview(restaurant, review)
        else OptionT[IO, Int](IO(Some(0)))
    } yield reviewsInserted

    io.value.attempt.unsafeRunSync()
  }

}




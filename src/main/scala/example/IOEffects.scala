package example

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}
import cats._
import cats.data._
import cats.effect.IO
import cats.effect.implicits._
import cats.implicits._

import scala.collection.mutable

object IOEffects {

  // Retrieving Records from data lake (S3)
  sealed trait Record
  case class Location(lat: Double, lng: Double) extends Record
  case class Restaurant(name: String, area: String) extends Record

  val restaurantsDataLake = mutable.Map(
    "soup" -> Restaurant("Soup Place", "Queens"),
    "kabob" -> Restaurant("Babu Bhatt's", "Brooklyn"),
    "diner" -> Restaurant("Tom's Diner", "Manhattan")
  )

  val locationsDataLake = mutable.Map(
    "Queens" -> Location(40.10, 74.11),
    "Brooklyn" -> Location(41.12, 79.00),
    "Manhattan" -> Location(44.00, 70.26)
  )

  val reviews = mutable.Map(
    Restaurant("Soup Place", "Queens") -> "The guy was really mean!"
  )

  // Notice that it is unsafe
  def fetch[K, Record](bucket: mutable.Map[K, Record], key: K): Record = {
    Thread.sleep(1000)
    bucket(key)
  }

  def write[K, Record](bucket: mutable.Map[K, Record], key: K, record: Record): Int = {
    Thread.sleep(1000)
    bucket.put(key, record)
    1
  }

  def findRestaurantJava(search: String): Location = {
    var restaurant: Restaurant = null
    var location: Location = null
    try {
      restaurant = fetch(restaurantsDataLake, search)
      if (restaurant != null) {
        location = fetch(locationsDataLake, restaurant.area)
      }
    } catch {
      case e: Exception => println("Fetch failed somewhere... we don't know")
    }
    location
  }

  // Example 1: Cats IO, fetch
  def ioFetch[K,Record](bucket: mutable.Map[K, Record], key: K): IO[Record] =
    IO(fetch(bucket, key))

  def findRestaurantScala(search: String): Location =
    ioFetch(restaurantsDataLake, search)
      .flatMap(r => ioFetch(locationsDataLake, r.area))
      .unsafeRunSync()

  // Example 2: Cats IO, fetch and write
  def ioWrite[K,Record](bucket: mutable.Map[K, Record], key: K, value: Record): IO[Int] =
    IO(write(bucket, key, value))

  def findReviewRestaurantScala(search: String): Int = {
    val program = for {
      restaurant <- ioFetch(restaurantsDataLake, search)
      location <- ioFetch(locationsDataLake, restaurant)
      reviewInserted <-
        if (location.lat > 40)
          ioWrite(reviews, restaurant, "Ugh, so far")
        else 0.pure[IO]
    } yield reviewInserted

    program.unsafeRunSync()
  }

  val x = ioWrite(restaurantsDataLake, "cherf", Restaurant("Bb", "sdf"))
}




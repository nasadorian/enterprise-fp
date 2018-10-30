package example

import cats.effect.IO
import cats.implicits._
import cats.data._

import scala.collection.mutable

object IOEffects {

  // Retrieving Records from data lake (S3)
  case class Location(name: String)
  case class Restaurant(name: String, location: Location)
  case class Review(review: String)

  val searchDatalake: mutable.Map[String, Restaurant] = mutable.Map(
    "soup" -> Restaurant("Soup Place", Location("Queens")),
    "kabob" -> Restaurant("Babu Bhatt's", Location("Brooklyn")),
    "diner" -> Restaurant("Tom's Diner", Location("Manhattan"))
  )

  val reviewsDatalake: mutable.Map[Restaurant, List[Review]] = mutable.Map(
    Restaurant("Soup Place", Location("Queens")) -> List(Review("Guy at counter was really rude.")),
    Restaurant("Babu Bhatt's", Location("Brooklyn")) -> List(Review("They changed their entire menu!"))
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

  // For convenience and reuse, we will wrap the original API in IO and OptionT
  def ioFetch[K,V](dl: mutable.Map[K, V], key: K): OptionT[IO,V] =
    OptionT[IO,V](IO(fetch(dl, key)))

  def ioWrite[K,V](bucket: mutable.Map[K, V], key: K, value: V): IO[Int] =
    IO(write(bucket, key, value))

  // Search is simply a matter of delegating to the fetch function
  def searchRestaurants(search: String): OptionT[IO, Restaurant] =
    ioFetch(searchDatalake, search)

  def insertReview(restaurant: Restaurant, review: Review): IO[Int] =
    for {
      reviews     <- ioFetch(reviewsDatalake, restaurant).value
      insert       = reviews.fold(List(review))(review :: _)
      numInserted <- ioWrite(reviewsDatalake, restaurant, insert)
    } yield numInserted

  // Tying it all together
  def searchAndReview(search: String, p: Restaurant => Boolean, review: Review): IO[Int] =
    for {
      restaurant  <- searchRestaurants(search).value
      inserted    <- restaurant.fold(0.pure[IO])(
        r => if (p(r)) insertReview(r, review) else 0.pure[IO])
    } yield inserted

  // A final tidbit using Traverse, which flips inner and outer layers
  def insertManyReviews(rr: List[(Restaurant, Review)]): IO[List[Int]] =
    rr.traverse {
      case (rest, rev) => insertReview(rest, rev)
    }


}




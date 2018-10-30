package example

import java.io.IOError

import cats.data.EitherT

import util.Random
import cats.effect.IO
import cats.effect.IO.Delay
import cats.effect.implicits._

import scala.concurrent.duration._

object Retry {

  type BackoffTime = Long
  type RetryConfig = List[BackoffTime]

  // Randomly fails
  def writeToCore: Boolean =
    if (Random.nextFloat() > .8) true
    else throw new IOError(new Throwable("Something went wrong"))

  val retries: RetryConfig = List.iterate(5L, 5)(i => i*i)

//  def retry[A](config: RetryConfig)(f: A): EitherT[IO, Throwable, A] =
//    EitherT(IO(f).unsafeRunTimed()
//      .leftFlatMap {
//        case t: IOError => retry(config.tail)(f)
//      }

}

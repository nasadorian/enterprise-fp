import cats.data._
import cats.implicits._


def foo[A,B,F[_]]: A => F[B]
def bar[B,C,F[_]]: B => F[C]

def kFoo = Kleisli(foo)
def kBar = Kleisli(bar)


kFoo.run

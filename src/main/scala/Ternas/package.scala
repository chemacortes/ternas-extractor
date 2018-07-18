import scala.util.Random
import scala.util.Try

package object Ternas {

  case class Terna[T](a: T, b: T, c: T)

  object TernaIntExtractor {
    def unapply(s: String): Option[Terna[Int]] =
      Try {
        val Array(x, y, z) = s.split(",").map(_.toInt)
        Terna(x, y, z)
      }.toOption
  }

  def mkTernas = (1 to 20) map { _ =>
    Terna(Random.nextInt(100), Random.nextInt(100), Random.nextInt(100))
  }

}
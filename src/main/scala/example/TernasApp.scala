package example

import scala.util.Random
import scala.util.Try

case class Terna[T](a: T, b: T, c: T)

object TernaIntExtractor {
  def unapply(s: String): Option[Terna[Int]] =
    Try {
      val Array(x, y, z) = s.split(",").map(_.toInt)
      Terna(x, y, z)
    }.toOption
}

object TernasApp extends App {

  // Generador de ternas como lista de strings
  // a la que se añaden dos errores
  val sample = "12,1\n" + // No es una terna
    ((1 to 30) map { _ =>
      s"${Random.nextInt(100)},${Random.nextInt(100)},${Random.nextInt(100)}"
    }).mkString("\n") +
    "Error\n"

  // Extracción de ternas
  val ternas = sample.split("\n") collect {
    case TernaIntExtractor(t) => t
  }

  ternas foreach println
  println("---")

  // Extracción y selección
  val sel = sample.split("\n") collect {
    case TernaIntExtractor(t @ Terna(_, 44, _)) => t
  }

  sel foreach println
  println("---")

  // Extracción y selección
  val sel2 = sample.split("\n") collect {
    case TernaIntExtractor(t) if t.b == 44 => t
  }

  sel2 foreach println
  println("---")

  // Selección, sin crear una nueva secuencia
  ternas withFilter {
    case Terna(_, 44, _) => true
    case _               => false
  } foreach println

  println("---")

  // Selección, sin crear una nueva secuencia
  for {
    t @ Terna(_, 44, _) <- ternas
  } println(t)

}


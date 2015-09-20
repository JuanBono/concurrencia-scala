package ejercicios.cap1

import scala.util.Try

object Capitulo1 {

  def compose1[A, B, C](g: B => C, f: A => B): A => C = f andThen g

  // Version con pattern matching
  def fuse[A, B](a: Option[A], b: Option[B]): Option[(A, B)] = (a, b) match {
    case (Some(v1), Some(v2)) => Some((v1, v2))
    case _ => None
  }

  // Version pedida por el enunciado usando for comprehension
  def fuse1[A, B](a: Option[A], b: Option[B]): Option[(A, B)] = for {
    a1 <- a
    b1 <- b
  } yield (a1, b1)

  def check[T](xs: Seq[T])(pred: T => Boolean): Boolean = Try(xs.forall(pred)) getOrElse false

}

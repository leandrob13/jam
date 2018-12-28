package jam.encoder

import jam.Yaml
import jam.Yaml._

import scala.collection.immutable.ListMap

trait Encoder[A] {

  def encode(v: A): Yaml
}

object Encoder extends EncoderDerivation {

  def instance[T](f: T => Yaml): Encoder[T] =
    f(_)

  implicit val booleanEncoder: Encoder[Boolean] =
    instance(if (_) YTrue else YFalse)

  implicit val stringEncoder: Encoder[String] =
    instance(YString)

  implicit val symbolEncoder: Encoder[Symbol] =
    instance(s => YString(s.name))

  implicit val intEncoder: Encoder[Int] =
    instance(i => YBigDecimal(BigDecimal(i)))

  implicit val longEncoder: Encoder[Long] =
    instance(i => YBigDecimal(BigDecimal(i)))

  implicit val floatEncoder: Encoder[Float] =
    instance(i => YBigDecimal(BigDecimal.decimal(i)))

  implicit val doubleEncoder: Encoder[Double] =
    instance(i => YBigDecimal(BigDecimal.decimal(i)))

  implicit val bigDecimalEncoder: Encoder[BigDecimal] =
    instance(i => YBigDecimal(i))

  implicit def optionEncoder[A](implicit enc: Encoder[A]): Encoder[Option[A]] =
    Encoder.instance {
      case None    => YNull
      case Some(t) => enc.encode(t)
    }

  implicit def listEncoder[A](implicit enc: Encoder[A]): Encoder[List[A]] =
    Encoder.instance(t => YArray(t.map(enc.encode).toVector))

  implicit def tuple2Encoder[A, B](implicit ts: ToString[A], enc: Encoder[B]): Encoder[(A, B)] =
    Encoder.instance(t => YMap(ListMap(ts.string(t._1) -> enc.encode(t._2))))

  implicit def mapEncoder[A, B](implicit ts: ToString[A], enc: Encoder[B]): Encoder[Map[A, B]] =
    Encoder.instance(t => YMap(ListMap(t.map(t => (ts.string(t._1), enc.encode(t._2))).toList: _*)))

  abstract class ToString[A] {
    def string(v: A): String
  }

  object ToString {

    def instance[T](f: T => String): ToString[T] =
      f(_)

    implicit val string: ToString[String] = instance(identity)

    implicit val intToString: ToString[Int] = instance(_.toString)

    implicit val longToString: ToString[Long] = instance(_.toString)

    implicit val doubleToString: ToString[Symbol] = instance(_.name)
  }
}

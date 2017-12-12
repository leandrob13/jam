package jam

import scala.collection.immutable.ListMap

sealed abstract class Yaml extends Product with Serializable

object Yaml {

  private[jam] final case object YNull extends Yaml
  private[jam] final case object YTrue extends Yaml
  private[jam] final case object YFalse extends Yaml
  private[jam] final case class YInt(v: Int) extends Yaml
  private[jam] final case class YLong(v: Long) extends Yaml
  private[jam] final case class YFloat(v: Float) extends Yaml
  private[jam] final case class YDouble(v: Double) extends Yaml
  private[jam] final case class YString(v: String) extends Yaml
  private[jam] final case class YArray(v: Vector[Yaml]) extends Yaml
  private[jam] final case class YMap(v: ListMap[String, Yaml]) extends Yaml
}

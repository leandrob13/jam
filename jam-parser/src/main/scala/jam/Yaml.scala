package jam

import scala.collection.immutable.ListMap

sealed abstract class Yaml extends Product with Serializable {

  def isPrimitive: Boolean = false
}

object Yaml {

  private[jam] final case object YNull extends Yaml {
    override def isPrimitive: Boolean = true
  }

  private[jam] final case object YTrue extends Yaml {
    override def isPrimitive: Boolean = true
  }

  private[jam] final case object YFalse extends Yaml {
    override def isPrimitive: Boolean = true
  }

  private[jam] final case class YInt(v: Int) extends Yaml {
    override def isPrimitive: Boolean = true
  }

  private[jam] final case class YLong(v: Long) extends Yaml {
    override def isPrimitive: Boolean = true
  }

  private[jam] final case class YBigInt(v: BigInt) extends Yaml {
    override def isPrimitive: Boolean = true
  }

  private[jam] final case class YFloat(v: Float) extends Yaml {
    override def isPrimitive: Boolean = true
  }

  private[jam] final case class YDouble(v: Double) extends Yaml {
    override def isPrimitive: Boolean = true
  }

  private[jam] final case class YBigDecimal(v: BigDecimal) extends Yaml {
    override def isPrimitive: Boolean = true
  }

  private[jam] final case class YString(v: String) extends Yaml {
    override def isPrimitive: Boolean = true
  }

  private[jam] final case class YArray(v: Vector[Yaml]) extends Yaml

  private[jam] final case class YMap(v: ListMap[String, Yaml]) extends Yaml
}

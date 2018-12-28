package jam

import jam.printer.YamlPrinter

import scala.collection.immutable.ListMap

sealed abstract class Yaml extends Product with Serializable {

  def print(p: YamlPrinter = YamlPrinter.notNulls): String =
    p.printYaml(this, new StringBuilder).result()
}

object Yaml {

  private[jam] final case object YNull extends Yaml

  private[jam] final case object YTrue extends Yaml

  private[jam] final case object YFalse extends Yaml

  private[jam] final case class YBigDecimal(v: BigDecimal) extends Yaml

  private[jam] final case class YString(v: String) extends Yaml

  private[jam] final case class YArray(v: Seq[Yaml]) extends Yaml

  private[jam] final case class YMap(v: ListMap[String, Yaml]) extends Yaml {

    def ++(m: YMap): YMap =
      YMap(v ++ m.v)

    def +(t: (String, Yaml)): YMap =
      YMap(v + t)
  }
}

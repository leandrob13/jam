package jam.parser

import jam.Yaml

object YamlPrinter {
  import Yaml._

  def printYMap(yMap: YMap, builder: StringBuilder, indent: Int = 0): scala.StringBuilder = {
    yMap.v.foldLeft(builder) {
      case (b, (k, y)) =>
        b.append(s"${"  " * indent}$k: ")
        printYaml(y, b, 1)
        b.append("\n")
    }
  }

  def printYArray(yArray: YArray, builder: StringBuilder): scala.StringBuilder = ???

  def printYaml(y: Yaml, builder: StringBuilder, indent: Int = 0): scala.StringBuilder =
    y match {
      case YNull          => builder.append("null")
      case YFalse         => builder.append("false")
      case YTrue          => builder.append("true")
      case YString(s)     => builder.append(s)
      case YBigDecimal(b) => builder.append(b.toString)
      case YBigInt(b)     => builder.append(b.toString)
      case m @ YMap(_)    => printYMap(m, builder, indent)
      case a @ YArray(_)  => printYArray(a, builder)
      case _              => builder
    }

}

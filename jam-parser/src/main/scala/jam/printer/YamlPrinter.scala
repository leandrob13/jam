package jam.printer

import jam.Yaml

object YamlPrinter {
  import Yaml._

  private[printer] def printYMap(
      yMap: YMap,
      builder: StringBuilder,
      indent: Int,
      first: Boolean = true
  ): scala.StringBuilder = {
    var f = first
    yMap.v.foldLeft(builder) {
      case (b, (k, y)) =>
        if (!f) b.append(s"\n${"  " * indent}")
        b.append(s"$k:")
        f = false
        printYaml(y, b, indent + 1, f)
    }
  }

  private[printer] def printYArray(yArray: YArray, builder: StringBuilder, indent: Int): scala.StringBuilder =
    yArray.v match {
      case vec @ _ +: _ =>
        vec.foldLeft(builder) {
          case (b, yaml) =>
            b.append("\n")
            b.append(s"${"  " * indent}- ")
            yaml match {
              case m @ YMap(_)        => printYMap(m, builder, indent + 1)
              case y if y.isPrimitive => printPrimitives(b, 0)(y)
            }
        }
      case _ =>
        builder.append(" []")

    }

  private[printer] def printPrimitives(builder: StringBuilder, space: Int = 0): PartialFunction[Yaml, StringBuilder] = {
    case YNull  => builder.append(s"${" " * space}null")
    case YFalse => builder.append(s"${" " * space}false")
    case YTrue  => builder.append(s"${" " * space}true")
    case YString(s) =>
      builder.append(s"${" " * space}")
      builder.append("\"")
      builder.append(s)
      builder.append("\"")
    case YBigDecimal(b) => builder.append(s"${" " * space}" + b.toString)
  }

  def printYaml(yaml: Yaml, builder: StringBuilder, indent: Int = 0, first: Boolean = true): scala.StringBuilder =
    yaml match {
      case m @ YMap(_)        => printYMap(m, builder, indent, first)
      case a @ YArray(_)      => printYArray(a, builder, indent)
      case y if y.isPrimitive => printPrimitives(builder, 1)(y)
      case _                  => builder
    }

}

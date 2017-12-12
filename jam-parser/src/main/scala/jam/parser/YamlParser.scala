package jam.parser

import fastparse.all._
import jam.Yaml

class YamlParser {

  val strings = P(AnyChar.rep)

  val digits = P(CharPred(_.isDigit).rep)

  val ints = digits.!.map(x => Yaml.YInt(x.toInt))

  val longs = digits.!.map(x => Yaml.YLong(x.toLong))

  val floats = P("." ~ digits).!.map(x => Yaml.YFloat(x.toFloat))

  val doubles = P("." ~ digits).!.map(x => Yaml.YDouble(x.toDouble))

}

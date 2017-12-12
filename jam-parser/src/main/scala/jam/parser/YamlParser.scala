package jam.parser

import fastparse.all._
import jam.Yaml

class YamlParser {

  val digits = P(CharIn("+-").? ~ CharPred(_.isDigit).rep)

  val decimals = P(digits ~ "." ~ digits ~ (CharIn("eE") ~ digits).?)

  val strings = P(AnyChar.rep).!.map(x => Yaml.YString(x))

  val ints = digits.!.map(x => Yaml.YInt(x.toInt))

  val longs = digits.!.map(x => Yaml.YLong(x.toLong))

  val floats = decimals.!.map(x => Yaml.YFloat(x.toFloat))

  val doubles = decimals.!.map(x => Yaml.YDouble(x.toDouble))

  val True = P("True" | "true").!.map(_ => Yaml.YTrue)

  val False = P("False" | "false").!.map(_ => Yaml.YFalse)

}

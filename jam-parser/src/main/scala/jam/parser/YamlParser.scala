package jam.parser

import fastparse.all
import fastparse.all._
import jam.Yaml
import jam.Yaml.YMap

import scala.collection.immutable.ListMap

class YamlParser {

  val stringChars: Char => Boolean = !":\n\"\\".contains(_: Char)

  val hexDigit = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))

  val unicodeEscape = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)

  val escape = P("\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape))

  val strChars = P(CharsWhile(stringChars))

  val space = P(CharsWhileIn(" \r\n").?)

  val digits = P(CharIn("+-").? ~ CharsWhileIn("0123456789"))

  val decimals = P(digits ~ "." ~ digits ~ (CharIn("eE") ~ digits).?)

  val keys = P((strChars | escape).rep).!

  val strings = P("\"".? ~/ (strChars | escape).rep.! ~ "\"".?).map(x => Yaml.YString(x))

  val ints = digits.!.map(x => Yaml.YInt(x.toInt))

  val longs = digits.!.map(x => Yaml.YLong(x.toLong))

  val floats = decimals.!.map(x => Yaml.YFloat(x.toFloat))

  val doubles = decimals.!.map(x => Yaml.YDouble(x.toDouble))

  val True = P("True" | "true").!.map(_ => Yaml.YTrue)

  val False = P("False" | "false").!.map(_ => Yaml.YFalse)

  val pair = P(keys ~/ ":" ~/ expr)

  val obj = P(pair.rep(sep = "\n".~/)).map(x => YMap(ListMap(x: _*)))

  val expr: all.Parser[Yaml] =
    P(" ".? ~ (True | False | ints | strings) ~ &("\n").?)

}

package jam.parser

import fastparse.all
import jam.Yaml
import jam.Yaml.{ YArray, YMap }

import scala.collection.immutable.ListMap

class YamlParser {
  import all._

  val stringChars: Char => Boolean = !(":\n\"\\").contains(_: Char)

  val hexDigit = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))

  val unicodeEscape = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)

  val escape = P("\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape))

  val strChars = P(CharsWhile(stringChars))

  val space = P(CharsWhileIn(" \r").?)

  val digits = P(CharIn("+-").? ~ CharsWhileIn("0123456789"))

  val decimals = P(digits ~ "." ~ digits ~ (CharIn("eE") ~ digits).?)

  val keys = P((strChars | escape).rep.! ~/ ":").log()

  val strings = P("\"".? ~/ (strChars | escape).rep.! ~ "\"".?).map(x => Yaml.YString(x)).log()

  val ints = digits.!.map(x => Yaml.YInt(x.toInt)).log()

  val longs = digits.!.map(x => Yaml.YLong(x.toLong))

  val floats = decimals.!.map(x => Yaml.YFloat(x.toFloat))

  val doubles = decimals.!.map(x => Yaml.YDouble(x.toDouble))

  val True = P("True" | "true").!.map(_ => Yaml.YTrue).log()

  val False = P("False" | "false").!.map(_ => Yaml.YFalse)

  val keyValue = P(keys ~ space ~/ expr).log()

  val nested = P("\n" ~ " ".rep.!).log()

  val array = P((Start | ("\n" ~ " ".rep)) ~ "-" ~ " ".rep ~/ expr.rep(sep = ("\n" ~ " ".rep ~ "-" ~ " ".rep).~/))
    .map(x => YArray(x.toVector))
    .log()

  val nArray = P(nested.flatMap(s => (("-" ~ " ".rep) ~/ expr).rep(sep = ("\n" + s).~/)))
    .map(x => YArray(x.toVector))
    .log()

  val nObj = P(nested.flatMap(s => !("-" ~ " ".rep) ~/ keyValue.rep(sep = ("\n" + s).~/)))
    .map(x => YMap(ListMap(x: _*)))
    .log()

  val obj = P(keyValue).map(x => YMap(ListMap(x))).log()

  val end = P(CharsWhileIn(" \r\n").rep.? ~ End)

  val expr: all.Parser[Yaml] =
    P((nObj | nArray | True | ints | strings) ~ end.?).log()
  //P(" ".? ~ (array | obj | True | False | ints | strings)).log()

}

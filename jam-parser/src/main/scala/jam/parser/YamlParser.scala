package jam.parser

import fastparse.{all, core}
import jam.Yaml
import jam.Yaml.{YArray, YMap}

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

  val nArray = P(nested.flatMap(s => (("-" ~ " ") ~/ expr).rep(sep = ("\n" + s).~/)))
    .map(x => YArray(x.toVector))
    .log()

  val arrayKeys2 = P(keys ~ space ~ nested ~ &("- ")).log()

  val array = P {
    val x = for {
      ak <- arrayKeys2
      (k, s) = ak
      e <- ("- " ~/ expr).rep(sep = ("\n" + s).~/)
    } yield YMap(ListMap(k -> YArray(e.toVector)))
    x //.rep(sep = "\n").map(x => x)
  }

  def objectRec(x: String = ""): all.Parser[(String, Yaml)] = P {
    for {
      a <- keys ~ space ~ nested.?
      (s, o) = a
      b <- o match {
        case None =>
          println(s"xxxxxxxx NO new line $s")
          primitives
        case Some(n) =>
          println(s"xxxxxxxx New line $s")
          rootKeys(n + x)
      }
    } yield (s, b)
  }

  def rootKeys(s: String = ""): core.Parser[YMap, Char, String] =
    P(arrayKeys(s).rep(sep = ("\n" + s).~/)).map(x => YMap(ListMap(x: _*))).log()

  def arrayKeys(s: String = "") = P {
    for {
      a <- &("- ").!.?
      b <- a match {
        case None =>
          println(s"xxxxxxxx NO ARRAY")
          objectRec()
        case Some(_) =>
          println(s"xxxxxxxx ARRAY $a")
          objectRec()
          //("- " ~/ rootKeys("  ")).rep(sep = ("\n" + s).~/).map(x => YArray(x.toVector))

      }
    } yield b
  }

  val nObj = P(((nested | PassWith("")) ~ !("-" ~ " ".rep)).flatMap(s => keyValue.rep(sep = ("\n" + s).~/)))
    .map(x => YMap(ListMap(x: _*)))
    .log()

  val end = P(CharsWhileIn(" \r\n").rep.? ~ End)

  val primitives = P(True | ints | strings).log()

  val expr: all.Parser[Yaml] =
    P((nObj | nArray | True | ints | strings) ~ end.?).log()

}

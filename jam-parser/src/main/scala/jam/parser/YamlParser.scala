package jam.parser

import fastparse.all
import jam.Yaml
import jam.Yaml._

import scala.collection.immutable.ListMap

object YamlParser {
  import all._

  val stringChars: Char => Boolean = !(":\n\"\\").contains(_: Char)

  val hexDigit = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))

  val unicodeEscape = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)

  val escape = P("\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape))

  val strChars = P(CharsWhile(stringChars))

  val digits = P(CharIn("+-").? ~ CharsWhileIn("0123456789"))

  val decimals = P(digits ~ "." ~ digits ~ (CharIn("eE") ~ digits).?)

  val strings = P("\"".? ~/ (strChars | escape).rep.! ~ "\"".?).map(x => Yaml.YString(x)).log()

  val ints = digits.!.map(x => Yaml.YInt(x.toInt)).log()

  val longs = digits.!.map(x => Yaml.YLong(x.toLong))

  val bigInts = digits.!.map(x => Yaml.YBigInt(BigInt(x)))

  val floats = decimals.!.map(x => Yaml.YFloat(x.toFloat))

  val doubles = decimals.!.map(x => Yaml.YDouble(x.toDouble))

  val bigDecimals = decimals.!.map(x => Yaml.YBigDecimal(BigDecimal(x)))

  val True = P("True" | "true").!.map(_ => Yaml.YTrue).log()

  val False = P("False" | "false").!.map(_ => Yaml.YFalse)

  val primitives = P(True | False | bigDecimals | doubles | floats | bigInts | longs | ints | strings).log()

  val space = P(CharsWhileIn(" \r").?)

  val keys = P((strChars | escape).rep.! ~/ ":").log()

  val nested = P("\n" ~ " ".rep.!).log()

  def root(s: String = ""): all.Parser[Yaml] =
    P {
      for {
        a <- &("- ").!.?
        b <- a match {
          case None =>
            objectRec.rep(sep = (("\n" + s) ~ !end).~/).map(x => YMap(ListMap(x: _*)))
          case Some(_) =>
            ("- " ~/ collectionRec(s + "  ")).rep(sep = (("\n" + s) ~ !end).~/).map(x => YArray(x.toVector))
        }
      } yield b
    }.log()

  def collectionRec(s: String = ""): all.Parser[Yaml] =
    P {
      for {
        a <- &(keys).!.?
        b <- a match {
          case None =>
            primitives
          case Some(k) =>
            objectRec.rep(sep = (("\n" + s) ~ !"- ").~/).map(x => YMap(ListMap(x: _*)))
        }
      } yield b
    }.log()

  val objectRec: all.Parser[(String, Yaml)] =
    P {
      for {
        a <- keys ~ space ~ nested.?
        (s, o) = a
        b <- o match {
          case None    => primitives
          case Some(n) => root(n)
        }
      } yield (s, b)
    }.log()

  val start = P(Start ~ CharsWhileIn(" \r\n").rep.?)

  val end = P(CharsWhileIn(" \r\n").rep.? ~ End)

  val yaml = start ~ root()

}

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

  val digits = P(CharIn("+-").? ~ CharsWhileIn("0123456789"))

  val decimals = P(digits ~ "." ~ digits ~ (CharIn("eE") ~ digits).?)

  val strings = P("\"".? ~/ (strChars | escape).rep.! ~ "\"".?).map(x => Yaml.YString(x)).log()

  val ints = digits.!.map(x => Yaml.YInt(x.toInt)).log()

  val longs = digits.!.map(x => Yaml.YLong(x.toLong))

  val floats = decimals.!.map(x => Yaml.YFloat(x.toFloat))

  val doubles = decimals.!.map(x => Yaml.YDouble(x.toDouble))

  val True = P("True" | "true").!.map(_ => Yaml.YTrue).log()

  val False = P("False" | "false").!.map(_ => Yaml.YFalse)

  val primitives = P(True | False | doubles | floats | longs | ints | strings).log()

  val space = P(CharsWhileIn(" \r").?)

  val keys = P((strChars | escape).rep.! ~/ ":").log()

  val nested = P("\n" ~ " ".rep.!).log()

  def root(s: String = ""): all.Parser[Yaml] =
    P {
      for {
        a <- &("- ").!.?
        b <- a match {
          case None =>
            println(s"xxxxxxxx NO ARRAY")
            objectRec().rep(sep = ("\n" + s).~/).map(x => YMap(ListMap(x: _*)))
          case Some(_) =>
            println(s"xxxxxxxx ARRAY ${s.length}")
            ("- " ~/ collectionRec(s)).rep(sep = ("\n" + s).~/).map(x => YArray(x.toVector))
        }
      } yield b
    }.log()

  def collectionRec(s: String = ""): all.Parser[Yaml] =
    P {
      for {
        a <- &(keys).!.?
        b <- a match {
          case None =>
            println(s"xxxxxxxx NO KEY")
            primitives
          case Some(k) =>
            println(s"xxxxxxxx With KEY $k")
            objectRec().rep(sep = (("\n" + s) ~ !"- ").~/).map(x => YMap(ListMap(x: _*)))
        }
      } yield b
    }.log()

  def objectRec(x: String = ""): all.Parser[(String, Yaml)] =
    P {
      for {
        a <- keys ~ space ~ nested.?
        (s, o) = a
        b <- o match {
          case None =>
            println(s"xxxxxxxx NO new line $s")
            primitives
          case Some(n) =>
            println(s"xxxxxxxx New line $s")
            root(n + x)
        }
      } yield (s, b)
    }.log()

  val end = P(CharsWhileIn(" \r\n").rep.? ~ End)

}

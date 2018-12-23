package jam.parser

import fastparse._
import NoWhitespace._
import jam.Yaml
import jam.Yaml._

import scala.collection.immutable.ListMap

object YamlParser {

  val stringChars: Char => Boolean = !":\n\"\\".contains(_: Char)

  def hexDigit[_: P]: P[Unit] = P(CharIn("0-9", "a-f", "A-F"))

  def unicodeEscape[_: P]: P[Unit] = P("u" ~ hexDigit.rep(exactly = 4) ~ End)

  def escape[_: P]: P[Unit] = P("\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape))

  def strChars[_: P]: P[Unit] = P(CharsWhile(stringChars))

  def digits[_: P]: P[Unit] = P(CharIn("\\+", "\\-").? ~ CharsWhileIn("0123456789"))

  def decimals[_: P]: P[Unit] = P(((digits ~ "." ~ digits) | digits) ~ (CharIn("eE") ~ digits).?)

  def strings[_: P]: P[YString] = P("\"".? ~/ (strChars | escape).rep.! ~ "\"".?).map(x => Yaml.YString(x))

  def bigDecimals[_: P]: P[YBigDecimal] = decimals.!.map(x => Yaml.YBigDecimal(BigDecimal(x)))

  def True[_: P]: P[Yaml.YTrue.type] = P("True" | "true").!.map(_ => Yaml.YTrue)

  def False[_: P]: P[Yaml.YFalse.type] = P("False" | "false").!.map(_ => Yaml.YFalse)

  def primitives[_: P]: P[Yaml] = P(True | False | bigDecimals | strings)

  def space[_: P]: P[Unit] = P(CharsWhileIn(" \r").?)

  def keys[_: P]: P[String] = P((strChars | escape).rep.! ~/ ":")

  def nested[_: P]: P[String] = P("\n" ~ " ".rep.!)

  def emptyArray[_: P]: P[YArray] = "[]".!.map(_ => YArray(Vector.empty))

  def root[_: P](s: String = ""): P[Yaml] =
    P {
      for {
        a <- &("- ").!.?
        b <- a match {
          case None =>
            objectRec.rep(sep = ("\n" + s) ~ !end).map(x => YMap(ListMap(x: _*)))
          case Some(_) =>
            ("- " ~/ collectionRec(s + "  ")).rep(sep = ("\n" + s) ~ !end).map(x => YArray(x.toVector))
        }
      } yield b
    }

  def collectionRec[_: P](s: String = ""): P[Yaml] =
    P {
      for {
        a <- &(keys).!.?
        b <- a match {
          case None =>
            primitives
          case Some(_) =>
            objectRec.rep(sep = ("\n" + s) ~ !"- ").map(x => YMap(ListMap(x: _*)))
        }
      } yield b
    }

  def objectRec[_: P]: P[(String, Yaml)] =
    P {
      for {
        a <- keys ~ space ~ nested.?
        (s, o) = a
        b <- o match {
          case None =>
            emptyArray | primitives
          case Some(n) =>
            root(n)
        }
      } yield (s, b)
    }

  def start[_: P]: P[Unit] = P(Start ~ CharsWhileIn(" \r\n").rep.?)

  def end[_: P]: P[Unit] = P(CharsWhileIn(" \r\n").rep.? ~ End)

  def yaml[_: P]: P[Yaml] = start ~ root()

}

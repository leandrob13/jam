package jam.parser

import fastparse._
import jam.Yaml._
import org.scalatest.{ Inside, MustMatchers, WordSpec }

import scala.collection.immutable.ListMap
import scala.io.Source

class YamlParserTest extends WordSpec with MustMatchers with Inside with Generators {

  "YamlParser" should {

    "parse a string" in {
      forAll(stringGen) { s: String =>
        inside(parse(s, YamlParser.strings(_))) {
          case Parsed.Success(v, _) =>
            v mustBe YString(s)
        }
      }
    }

    "parse a positive BigDecimal" in {
      forAll(positiveBigDecimalGen) { s: BigDecimal =>
        inside(parse(s.toString, YamlParser.bigDecimals(_))) {
          case Parsed.Success(v, _) =>
            v mustBe YBigDecimal(s)
        }
      }
    }

    "parse a negative BigDecimal" in {
      forAll(negativeBigDecimalGen) { s: BigDecimal =>
        inside(parse(s.toString, YamlParser.bigDecimals(_))) {
          case Parsed.Success(v, _) =>
            v mustBe YBigDecimal(s)
        }
      }
    }

    "parse a valid hex digit" in {
      parse("f", YamlParser.hexDigit(_)) mustBe a[Parsed.Success[_]]
      parse("F", YamlParser.hexDigit(_)) mustBe a[Parsed.Success[_]]
      parse("9", YamlParser.hexDigit(_)) mustBe a[Parsed.Success[_]]
    }

    "parse an invalid hex digit" in {
      parse("J", YamlParser.hexDigit(_)) mustBe a[Parsed.Failure]
      parse("Z", YamlParser.hexDigit(_)) mustBe a[Parsed.Failure]
    }

    "parse a valid unicode sequence" in {
      parse("uabcd", YamlParser.unicodeEscape(_)) mustBe a[Parsed.Success[_]]
      parse("uABC4", YamlParser.unicodeEscape(_)) mustBe a[Parsed.Success[_]]
      parse("uA9bF", YamlParser.unicodeEscape(_)) mustBe a[Parsed.Success[_]]
      parse("u1234", YamlParser.unicodeEscape(_)) mustBe a[Parsed.Success[_]]
    }

    "parse a valid escape character" in {
      parse("\\n", YamlParser.escape(_)) mustBe a[Parsed.Success[_]]
      parse("\\b", YamlParser.escape(_)) mustBe a[Parsed.Success[_]]
      parse("\\r", YamlParser.escape(_)) mustBe a[Parsed.Success[_]]
    }

    "parse an invalid escape character" in {
      parse("\\w", YamlParser.escape(_)) mustBe a[Parsed.Failure]
    }

    "parse a valid character" in {
      parse("z", YamlParser.strChars(_)) mustBe a[Parsed.Success[_]]
      parse("5", YamlParser.strChars(_)) mustBe a[Parsed.Success[_]]
      parse("?", YamlParser.strChars(_)) mustBe a[Parsed.Success[_]]
    }

    "parse an invalid character" in {
      parse(":", YamlParser.strChars(_)) mustBe a[Parsed.Failure]
      parse("\\n", YamlParser.strChars(_)) mustBe a[Parsed.Failure]
    }

    "parse a valid number" in {
      parse("-24535345345", YamlParser.digits(_)) mustBe a[Parsed.Success[_]]
      parse("+500", YamlParser.digits(_)) mustBe a[Parsed.Success[_]]
      parse("679", YamlParser.digits(_)) mustBe a[Parsed.Success[_]]
    }

    "parse an invalid number" in {
      parse("-abc", YamlParser.digits(_)) mustBe a[Parsed.Failure]
    }

    "parse a valid decimal number" in {
      parse("-2.22", YamlParser.decimals(_)) mustBe a[Parsed.Success[_]]
      parse("+437.023424", YamlParser.decimals(_)) mustBe a[Parsed.Success[_]]
      parse("1.001", YamlParser.decimals(_)) mustBe a[Parsed.Success[_]]
      parse("1.001e-06", YamlParser.decimals(_)) mustBe a[Parsed.Success[_]]
      parse("1.001e+06", YamlParser.decimals(_)) mustBe a[Parsed.Success[_]]
      parse("1.001e06", YamlParser.decimals(_)) mustBe a[Parsed.Success[_]]
    }

    "parse an invalid decimal number" in {
      parse("--5432312312", YamlParser.decimals(_)) mustBe a[Parsed.Failure]
    }

    "fail parsing an invalid character range in an unicode sequence" in {
      parse("ufgej", YamlParser.unicodeEscape(_)) mustBe a[Parsed.Failure]
    }

    "fail parsing an invalid unicode sequence (length)" in {
      parse("uaaaab", YamlParser.unicodeEscape(_)) mustBe a[Parsed.Failure]
    }

    "fail parsing an invalid unicode sequence (invalid prefix)" in {
      parse("zaaaab", YamlParser.unicodeEscape(_)) mustBe a[Parsed.Failure]
    }

    "parse a true" in {
      inside(parse("true", YamlParser.True(_))) {
        case Parsed.Success(v, _) =>
          v mustBe YTrue
      }

      inside(parse("True", YamlParser.True(_))) {
        case Parsed.Success(v, _) =>
          v mustBe YTrue
      }
    }

    "parse a false" in {
      inside(parse("false", YamlParser.False(_))) {
        case Parsed.Success(v, _) =>
          v mustBe YFalse
      }

      inside(parse("False", YamlParser.False(_))) {
        case Parsed.Success(v, _) =>
          v mustBe YFalse
      }
    }

    "parse a simple object" in {
      val value = getYaml("/simple.yaml")

      inside(parse(value, YamlParser.yaml(_))) {
        case Parsed.Success(v, _) =>
          v mustBe YMap(
            ListMap(
              "name"    -> YString("Ralph"),
              "age"     -> YBigDecimal(33),
              "married" -> YTrue
            )
          )
      }
    }

    "parse a nested object" in {
      val value = getYaml("/nestedObject.yaml")

      inside(parse(value, YamlParser.yaml(_))) {
        case Parsed.Success(v, _) =>
          v mustBe YMap(
            ListMap(
              "info" -> YMap(
                ListMap(
                  "address"   -> YString("none"),
                  "telephone" -> YString("12345"),
                  "info" -> YMap(
                    ListMap(
                      "address"   -> YString("none"),
                      "telephone" -> YString("12345")
                    )
                  )
                )
              ),
              "test" -> YBigDecimal(0)
            )
          )
      }
    }

    "parse a nested array object" in {
      val value = getYaml("/nestedArrays.yaml")

      inside(parse(value, YamlParser.yaml(_))) {
        case Parsed.Success(v, _) =>
          v mustBe YMap(
            ListMap(
              "numbers" -> YArray(
                Vector(YBigDecimal(-1), YBigDecimal(2), YBigDecimal(3))
              ),
              "empty" -> YArray(Vector()),
              "details" -> YArray(
                Vector(
                  YMap(
                    ListMap(
                      "count"     -> YBigDecimal(-1),
                      "something" -> YTrue,
                      "array" -> YArray(
                        Vector(
                          YMap(
                            ListMap(
                              "count" -> YBigDecimal(1),
                              "name"  -> YString("james"),
                              "object" -> YMap(
                                ListMap(
                                  "kind"   -> YString("nothing to see"),
                                  "double" -> YBigDecimal(-0.001)
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  YMap(ListMap("count" -> YBigDecimal(2)))
                )
              )
            )
          )
      }
    }
  }

  def getYaml(path: String): String =
    Source
      .fromInputStream(this.getClass.getResourceAsStream(path))
      .mkString

}

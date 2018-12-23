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

    "parse a valid unicode sequence" in {
      parse("uabcd", YamlParser.unicodeEscape(_)) mustBe a[Parsed.Success[_]]
      parse("uABC4", YamlParser.unicodeEscape(_)) mustBe a[Parsed.Success[_]]
      parse("uA9bF", YamlParser.unicodeEscape(_)) mustBe a[Parsed.Success[_]]
      parse("u1234", YamlParser.unicodeEscape(_)) mustBe a[Parsed.Success[_]]
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

    "generate yaml" in {
      val sb = new StringBuilder()
      val yaml = YMap(
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
      val value = YamlPrinter.printYaml(yaml, sb).toString()
      value mustBe getYaml("/nestedObject.yaml")

      val yaml2 = YMap(
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
      val sb2    = new StringBuilder()
      val value2 = YamlPrinter.printYaml(yaml2, sb2).toString()
      value2 mustBe getYaml("/nestedArrays.yaml")
    }
  }

  def getYaml(path: String): String =
    Source
      .fromInputStream(this.getClass.getResourceAsStream(path))
      .mkString

}

package jam.parser

import fastparse.core.Parsed
import jam.Yaml._
import org.scalatest.{ Inside, MustMatchers, WordSpec }

import scala.collection.immutable.ListMap
import scala.io.Source

class YamlParserTest extends WordSpec with MustMatchers with Inside with Generators {

  val parser = YamlParser

  "YamlParser" should {

    "parse a string" in {

      forAll(stringGen) { s: String =>
        inside(parser.strings.parse(s)) {
          case Parsed.Success(v, _) =>
            v mustBe YString(s)
        }
      }
    }

    "parse a positive BigDecimal" in {
      forAll(positiveBigDecimalGen) { s: BigDecimal =>
        inside(parser.bigDecimals.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe YBigDecimal(s)
        }
      }
    }

    "parse a negative BigDecimal" in {
      forAll(negativeBigDecimalGen) { s: BigDecimal =>
        inside(parser.bigDecimals.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe YBigDecimal(s)
        }
      }
    }

    "parse a true" in {
      inside(parser.True.parse("true")) {
        case Parsed.Success(v, _) =>
          v mustBe YTrue
      }

      inside(parser.True.parse("True")) {
        case Parsed.Success(v, _) =>
          v mustBe YTrue
      }
    }

    "parse a false" in {
      inside(parser.False.parse("false")) {
        case Parsed.Success(v, _) =>
          v mustBe YFalse
      }

      inside(parser.False.parse("False")) {
        case Parsed.Success(v, _) =>
          v mustBe YFalse
      }
    }

    "parse a simple object" in {
      val value = getYaml("/simple.yaml")

      inside(parser.yaml.parse(value)) {
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

      inside(parser.yaml.parse(value)) {
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

      inside(parser.yaml.parse(value)) {
        case Parsed.Success(v, _) =>
          v mustBe YMap(
            ListMap(
              "numbers" -> YArray(
                Vector(YBigDecimal(-1), YBigDecimal(2), YBigDecimal(3))
              ),
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

  def getYaml(path: String) =
    Source
      .fromInputStream(this.getClass.getResourceAsStream(path))
      .mkString

}

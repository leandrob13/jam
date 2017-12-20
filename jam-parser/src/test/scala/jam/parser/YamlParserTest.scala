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

    "parse a positive Int" in {
      forAll(positiveIntGen) { s: Int =>
        inside(parser.ints.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe YInt(s)
        }
      }
    }

    "parse a negative Int" in {
      forAll(negativeIntGen) { s: Int =>
        inside(parser.ints.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe YInt(s)
        }
      }
    }

    "parse a positive Long" in {
      forAll(positiveLongGen) { s: Long =>
        inside(parser.longs.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe YLong(s)
        }
      }
    }

    "parse a negative Long" in {
      forAll(negativeLongGen) { s: Long =>
        inside(parser.longs.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe YLong(s)
        }
      }
    }

    "parse a positive Float" in {
      forAll(positiveFloatGen) { s: Float =>
        inside(parser.floats.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe YFloat(s)
        }
      }
    }

    "parse a negative Float" in {
      forAll(negativeFloatGen) { s: Float =>
        inside(parser.floats.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe YFloat(s)
        }
      }
    }

    "parse a positive Double" in {
      forAll(positiveDoubleGen) { s: Double =>
        inside(parser.doubles.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe YDouble(s)
        }
      }
    }

    "parse a negative Double" in {
      forAll(negativeDoubleGen) { s: Double =>
        inside(parser.doubles.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe YDouble(s)
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
              "age"     -> YBigInt(33),
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
              "test" -> YBigInt(0)
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
                Vector(YBigInt(-1), YBigInt(2), YBigInt(3))
              ),
              "details" -> YArray(
                Vector(
                  YMap(
                    ListMap(
                      "count"     -> YBigInt(-1),
                      "something" -> YTrue,
                      "array" -> YArray(
                        Vector(
                          YMap(
                            ListMap(
                              "count" -> YBigInt(1),
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
                  YMap(ListMap("count" -> YBigInt(2)))
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
          "test" -> YBigInt(0)
        )
      )
      val value = YamlPrinter.printYaml(yaml, sb).toString()
      value mustBe getYaml("/nestedObject.yaml")

      val yaml2 = YMap(
        ListMap(
          "numbers" -> YArray(
            Vector(YBigInt(-1), YBigInt(2), YBigInt(3))
          ),
          "details" -> YArray(
            Vector(
              YMap(
                ListMap(
                  "count"     -> YBigInt(-1),
                  "something" -> YTrue,
                  "array" -> YArray(
                    Vector(
                      YMap(
                        ListMap(
                          "count" -> YBigInt(1),
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
              YMap(ListMap("count" -> YBigInt(2)))
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

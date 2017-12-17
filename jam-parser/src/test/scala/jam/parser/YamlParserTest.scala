package jam.parser

import fastparse.core.Parsed
import jam.Yaml._
import org.scalatest.{ Inside, MustMatchers, WordSpec }

import scala.collection.immutable.ListMap
import scala.io.Source

class YamlParserTest extends WordSpec with MustMatchers with Inside with Generators {

  val parser = new YamlParser

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

      inside(parser.root().parse(value)) {
        case Parsed.Success(v, _) =>
          v mustBe YMap(
            ListMap(
              "name"    -> YString("Ralph"),
              "age"     -> YLong(33),
              "married" -> YTrue
            )
          )
      }
    }

    "parse a nested object" in {
      val value = getYaml("/nestedObject.yaml")

      inside(parser.root().parse(value)) {
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
              "test" -> YLong(0)
            )
          )
      }
    }

    "parse a nested array object" in {
      val value = getYaml("/nestedArrays.yaml")

      println(s"==========OBJ1 ${parser.root().parse(value)}")
      inside(parser.root().parse(value)) {
        case Parsed.Success(v, _) =>
          v mustBe YMap(
            ListMap(
              "numbers" -> YArray(
                Vector(YLong(1), YLong(2), YLong(3))
              ),
              "details" -> YArray(
                Vector(
                  YMap(
                    ListMap("count" -> YLong(1))
                  ),
                  YMap(
                    ListMap("count" -> YLong(2))
                  )
                )
              )
            )
          )
      }
    }
  }

  def getYaml(path: String) =
    Source
      .fromInputStream(this.getClass.getResourceAsStream(path))
      .mkString

}

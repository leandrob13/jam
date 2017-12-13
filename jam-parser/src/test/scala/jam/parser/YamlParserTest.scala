package jam.parser

import fastparse.core.Parsed
import jam.Yaml
import jam.Yaml.{ YInt, YMap, YString, YTrue }
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
            v mustBe Yaml.YString(s)
        }
      }

    }

    "parse a positive Int" in {
      forAll(positiveIntGen) { s: Int =>
        inside(parser.ints.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe Yaml.YInt(s)
        }
      }
    }

    "parse a negative Int" in {
      forAll(negativeIntGen) { s: Int =>
        inside(parser.ints.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe Yaml.YInt(s)
        }
      }
    }

    "parse a positive Long" in {
      forAll(positiveLongGen) { s: Long =>
        inside(parser.longs.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe Yaml.YLong(s)
        }
      }
    }

    "parse a negative Long" in {
      forAll(negativeLongGen) { s: Long =>
        inside(parser.longs.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe Yaml.YLong(s)
        }
      }
    }

    "parse a positive Float" in {
      forAll(positiveFloatGen) { s: Float =>
        inside(parser.floats.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe Yaml.YFloat(s)
        }
      }
    }

    "parse a negative Float" in {
      forAll(negativeFloatGen) { s: Float =>
        inside(parser.floats.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe Yaml.YFloat(s)
        }
      }
    }

    "parse a positive Double" in {
      forAll(positiveDoubleGen) { s: Double =>
        inside(parser.doubles.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe Yaml.YDouble(s)
        }
      }
    }

    "parse a negative Double" in {
      forAll(negativeDoubleGen) { s: Double =>
        inside(parser.doubles.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe Yaml.YDouble(s)
        }
      }
    }

    "parse a true" in {
      inside(parser.True.parse("true")) {
        case Parsed.Success(v, _) =>
          v mustBe Yaml.YTrue
      }

      inside(parser.True.parse("True")) {
        case Parsed.Success(v, _) =>
          v mustBe Yaml.YTrue
      }
    }

    "parse a false" in {
      inside(parser.False.parse("false")) {
        case Parsed.Success(v, _) =>
          v mustBe Yaml.YFalse
      }

      inside(parser.False.parse("False")) {
        case Parsed.Success(v, _) =>
          v mustBe Yaml.YFalse
      }
    }

    "parse an object" in {
      val value = getYaml("/simple.yaml")

      inside(parser.obj.parse(value)) {
        case Parsed.Success(v, 35) =>
          v mustBe YMap(
            ListMap(
              "name"    -> YString("Ralph"),
              "age"     -> YInt(33),
              "married" -> YTrue
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

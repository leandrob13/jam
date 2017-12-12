package jam.parser

import fastparse.core.Parsed
import jam.Yaml
import org.scalatest.{ Inside, MustMatchers, WordSpec }

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

    "parse a positive Double" in {
      forAll(positiveDoubleGen) { s: Double =>
        println(s"============$s")
        inside(parser.doubles.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            v mustBe Yaml.YDouble(s)
        }
      }
    }

    "parse a negative Double" in {
      forAll(negativeDoubleGen) { s: Double =>
        println(s"============$s")
        inside(parser.doubles.parse(s.toString)) {
          case Parsed.Success(v, _) =>
            println(s"=================V $v")
            println(s"=================XXXXX ${parser.doubles.parse(s.toString)}")
            v mustBe Yaml.YDouble(s)
        }
      }
    }
  }

}

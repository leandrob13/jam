package jam.encoder

import jam.Yaml
import jam.Yaml._
import org.scalatest.{ MustMatchers, WordSpec }

import scala.collection.immutable.ListMap
import scala.io.Source

class EncoderTest extends WordSpec with MustMatchers {
  import EncoderTest._

  "Encoder" should {
    "encode primitives" in {
      val res: Yaml = Encoder[Primitives].encode(Primitives())
      res mustBe YMap(
        ListMap(
          "int"     -> YBigDecimal(1),
          "long"    -> YBigDecimal(2),
          "float"   -> YBigDecimal(3.3),
          "double"  -> YBigDecimal(4.4),
          "boolean" -> YTrue
        )
      )
    }

    "encode composed types" in {
      val c = Composed(Some(Primitives()), List(1, 2, -3), List("one", "two"))

      val res: Yaml = Encoder[Composed].encode(c)

      res mustBe YMap(
        ListMap(
          "primitives" -> YMap(
            ListMap(
              "int"     -> YBigDecimal(1),
              "long"    -> YBigDecimal(2),
              "float"   -> YBigDecimal(3.3),
              "double"  -> YBigDecimal(4.4),
              "boolean" -> YTrue
            )
          ),
          "numbers" -> YArray(Vector(YBigDecimal(1), YBigDecimal(2), YBigDecimal(-3))),
          "tags"    -> YArray(Vector(YString("one"), YString("two")))
        )
      )

      val res2: Yaml = Encoder[Composed].encode(c.copy(primitives = None, numbers = Nil))

      res2 mustBe YMap(
        ListMap(
          "primitives" -> YNull,
          "numbers"    -> YArray(Vector()),
          "tags"       -> YArray(Vector(YString("one"), YString("two")))
        )
      )
    }

    "encode a Map" in {
      val m: Map[String, Map[String, List[String]]] = Map(
        "props" -> Map(
          "sources" -> List("one", "two", "three")
        )
      )

      val res: Yaml = Encoder[Map[String, Map[String, List[String]]]].encode(m)

      res mustBe YMap(
        ListMap(
          "props" -> YMap(
            ListMap(
              "sources" -> YArray(Vector(YString("one"), YString("two"), YString("three")))
            )
          )
        )
      )
    }

    "encode a Tuple2" in {
      val t = "data" -> "name"

      val res: Yaml = Encoder[(String, String)].encode(t)

      res mustBe YMap(ListMap("data" -> YString("name")))
    }

    "encode and print a class" in {
      // Support for self recursive types
      val test = TestYaml(
        info = Info("none", "12345", Some(Info("none", "12345"))),
        test = BigDecimal(0)
      )

      val yaml  = Encoder[TestYaml].encode(test)
      val value = yaml.print()
      value mustBe getYaml("/nestedObject.yaml")
    }
  }
}

object EncoderTest {

  case class TestYaml(
      info: Info,
      test: BigDecimal
  )

  case class Info(
      address: String,
      telephone: String,
      info: Option[Info] = None
  )

  case class Primitives(
      int: Int = 1,
      long: Long = 2L,
      float: Float = 3.3f,
      double: Double = 4.4,
      boolean: Boolean = true
  )

  case class Composed(primitives: Option[Primitives], numbers: List[Long], tags: List[String])

  def getYaml(path: String): String =
    Source
      .fromInputStream(this.getClass.getResourceAsStream(path))
      .mkString
}

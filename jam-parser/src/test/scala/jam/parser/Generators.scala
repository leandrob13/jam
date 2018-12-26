package jam.parser

import org.scalacheck.Gen.listOf
import org.scalacheck.{ Arbitrary, Gen }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait Generators extends GeneratorDrivenPropertyChecks {

  val arbitraryString: Arbitrary[String] = Arbitrary.arbString

  val arbitraryDouble: Arbitrary[Double] = Arbitrary.arbDouble

  val arbitraryFloat: Arbitrary[Float] = Arbitrary.arbFloat

  val arbitraryBigInt: Arbitrary[BigInt] = Arbitrary.arbBigInt

  val arbitraryBigDecimal: Arbitrary[BigDecimal] = Arbitrary.arbBigDecimal

  val stringGen: Gen[String] =
    listOf(Gen.frequency((5, Gen.alphaNumStr), (3, " "))).map(_.mkString)

  val negativeIntGen: Gen[Int] = Gen.chooseNum[Int](-2147483648, -1)

  val positiveIntGen: Gen[Int] = Gen.chooseNum[Int](0, 2147483647)

  val negativeLongGen: Gen[Long] = Gen.choose[Long](-9223372036854775808L, -1L)

  val positiveLongGen: Gen[Long] = Gen.chooseNum[Long](0L, 9223372036854775807L)

  val positiveFloatGen: Gen[Float] = arbitraryFloat.arbitrary.filter(_ >= 0D)

  val negativeFloatGen: Gen[Float] = arbitraryFloat.arbitrary.filter(_ < 0D)

  val positiveDoubleGen: Gen[Double] = arbitraryDouble.arbitrary.filter(_ >= 0D)

  val negativeDoubleGen: Gen[Double] = arbitraryDouble.arbitrary.filter(_ < 0D)

  val positiveBigDecimalGen: Gen[BigDecimal] = arbitraryBigDecimal.arbitrary.filter(_ >= 0)

  val negativeBigDecimalGen: Gen[BigDecimal] = arbitraryBigDecimal.arbitrary.filter(_ < 0)

  val positiveBigIntGen: Gen[BigInt] = arbitraryBigInt.arbitrary.filter(_ >= 0)

  val negativeBigIntGen: Gen[BigInt] = arbitraryBigInt.arbitrary.filter(_ < 0)

}

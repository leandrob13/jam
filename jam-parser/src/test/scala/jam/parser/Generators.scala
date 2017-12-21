package jam.parser

import org.scalacheck.Gen.listOf
import org.scalacheck.{ Arbitrary, Gen }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait Generators extends GeneratorDrivenPropertyChecks {

  val arbitraryString = Arbitrary.arbString

  val arbitraryDouble = Arbitrary.arbDouble

  val arbitraryFloat = Arbitrary.arbFloat

  val arbitraryBigInt = Arbitrary.arbBigInt

  val arbitraryBigDecimal = Arbitrary.arbBigDecimal

  val stringGen =
    listOf(Gen.frequency((5, Gen.alphaNumStr), (3, " "))).map(_.mkString)

  val negativeIntGen = Gen.chooseNum[Int](-2147483648, -1)

  val positiveIntGen = Gen.chooseNum[Int](0, 2147483647)

  val negativeLongGen = Gen.choose[Long](-9223372036854775808L, -1L)

  val positiveLongGen: Gen[Long] = Gen.chooseNum[Long](0L, 9223372036854775807L)

  val positiveFloatGen = arbitraryFloat.arbitrary.filter(_ >= 0D)

  val negativeFloatGen = arbitraryFloat.arbitrary.filter(_ < 0D)

  val positiveDoubleGen = arbitraryDouble.arbitrary.filter(_ >= 0D)

  val negativeDoubleGen = arbitraryDouble.arbitrary.filter(_ < 0D)

  val positiveBigDecimalGen = arbitraryBigDecimal.arbitrary.filter(_ >= 0)

  val negativeBigDecimalGen = arbitraryBigDecimal.arbitrary.filter(_ < 0)

  val positiveBigIntGen = arbitraryBigInt.arbitrary.filter(_ >= 0)

  val negativeBigIntGen = arbitraryBigInt.arbitrary.filter(_ < 0)

}

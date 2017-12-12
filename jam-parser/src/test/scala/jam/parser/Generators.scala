package jam.parser

import org.scalacheck.Gen.listOf
import org.scalacheck.{ Arbitrary, Gen }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait Generators extends GeneratorDrivenPropertyChecks {

  val stringGen =
    listOf(Gen.frequency((5, Gen.alphaNumStr), (3, " "), (2, Arbitrary.arbString.arbitrary))).map(_.mkString)

  val arbitraryString = Arbitrary.arbString

  val arbitraryDouble = Arbitrary.arbDouble

  //val digitGen = listOf(Gen.numChar.filter(_.isDigit)).map(_.mkString).suchThat(_.nonEmpty)
  val negativeIntGen = Gen.chooseNum[Int](-2147483648, -1)

  val positiveIntGen = Gen.chooseNum[Int](0, 2147483647)

  val positiveDoubleGen = arbitraryDouble.arbitrary.filter(_ >= 0D)

  val negativeDoubleGen = arbitraryDouble.arbitrary.filter(_ < 0D)
  //.map(_.toString)
  //.suchThat(x => x.nonEmpty && x.forall(_.isDigit)) //Gen.numStr.suchThat(x => x.nonEmpty && x.forall(_.isDigit)) //arbitraryString.arbitrary.suchThat(_.forall(_.isDigit))
}

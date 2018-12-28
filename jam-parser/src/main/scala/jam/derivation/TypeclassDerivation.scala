package jam.derivation

import magnolia._
import scala.language.experimental.macros

trait TypeclassDerivation[C[_]] {

  type Typeclass[T] = C[T]

  def apply[T](implicit c: C[T]): C[T] = c

  def combine[T](caseClass: CaseClass[C, T]): C[T]

  def dispatch[T](sealedTrait: SealedTrait[C, T]): C[T]

  implicit def gen[T]: C[T] = macro Magnolia.gen[T]

}

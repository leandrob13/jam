package jam.encoder

import jam.Yaml.YMap
import jam.derivation.TypeclassDerivation
import magnolia.{ CaseClass, SealedTrait }

import scala.collection.immutable.ListMap

// $COVERAGE-OFF$
trait EncoderDerivation extends TypeclassDerivation[Encoder] {

  def combine[T](ctx: CaseClass[Encoder, T]): Encoder[T] = Encoder.instance { t =>
    YMap(
      ListMap(ctx.parameters.toList.map(p => (p.label, p.typeclass.encode(p.dereference(t)))): _*)
    )
  }

  def dispatch[T](ctx: SealedTrait[Encoder, T]): Encoder[T] = Encoder.instance { t =>
    ctx.dispatch(t) { sub =>
      sub.typeclass.encode(sub.cast(t))
    }
  }
}
// $COVERAGE-ON$

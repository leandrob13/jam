package jam.swagger
import jam.Yaml

object Parameter {
  sealed trait In

  object In {
    case object Path   extends In
    case object Query  extends In
    case object Header extends In
    case object Form   extends In
  }
}

trait Parameter {
  def name: String
  def required: Boolean
  def description: Option[String]
  def mandatory: Parameter
  val schema: Option[Yaml] = None

  protected def setType[T](t: T): Parameter

  def as[T <: AnyVal](implicit ev: T): Parameter = setType(ev)
}

case class BodyParameter(
  description: Option[String] = None,
  required: Boolean = false,
  name: String = "body"
) extends Parameter {
  override def mandatory                = copy(required = true)
  override def setType[T](t: T): Parameter = this
}

case class ArrayParameter[T <: AnyVal](
  name: String,
  required: Boolean = false,
  in: Parameter.In,
  description: Option[String] = None,
  itemType: T,
  format: Option[Format] = None)
  extends OperationParameter {

  def setType[T <: Type](t: T) = copy(`itemType` = t)

  override def mandatory = copy(required = true)
}

case class Parameter(name: String,
  required: Boolean = false,
  in: OperationParameter.In,
  description: Option[String] = None,
  `type`: Type = Type.String,
  format: Option[Format] = None)
  extends OperationParameter {

  def setType[T <: Type](t: T) = copy(`type` = t)
  override def mandatory       = copy(required = true)
}

object Parameter {
  def query(name: String,
    required: Boolean = false,
    description: Option[String] = None,
    `type`: Type = Type.String,
    format: Option[Format] = None) =
    apply(
      name,
      required = false,
      OperationParameter.In.Query,
      description,
      `type`,
      format
    )

  def path(name: String,
    description: Option[String] = None,
    `type`: Type = Type.String,
    format: Option[Format] = None,
    default: Option[String] = None) =
    apply(
      name,
      required = true,
      OperationParameter.In.Path,
      description,
      `type`,
      format
    )
}

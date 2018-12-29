package jam.swagger

case class ApiSchema(
  info: ApiSchema.Info,
  host: String,
  basePath: String,
  swagger: String = "2.0",
  paths: Seq[ApiSchema.Path] = Seq.empty,
  parameters: OperationParameters = OperationParameters(Nil),
  schemes: Set[Scheme] = Set.empty,
  consumes: Set[String] = Set.empty,
  produces: Set[String] = Set.empty,
  definitions: Definitions = Definitions.empty,
  securityDefinitions: SecurityDefinitions = SecurityDefinitions.empty
)

object ApiSchema {
  object Info {
    case class Contact(name: Option[String] = None,
      url: Option[String] = None,
      email: Option[String] = None)

    case class License(name: String, url: Option[String] = None)
  }

  case class Info(title: String,
    version: String = "1.0",
    description: Option[String] = None,
    termsOfService: Option[String] = None,
    contact: Option[Info.Contact] = None,
    license: Option[Info.License] = None
  )

  case class Path(id: String,
    parameters: List[OperationParameter] = Nil,
    operations: Map[Method, Operation] = Map.empty)
}

package jam.swagger

case class ApiSchema(
    info: Info,
    host: String,
    basePath: String,
    swagger: String = "2.0",
    paths: Paths = Map.empty,
    schemes: Set[Scheme] = Set.empty,
    consumes: Set[String] = Set.empty,
    produces: Set[String] = Set.empty,
    definitions: Set[Definition] = Set.empty,
)

case class Info(
    title: String,
    version: String = "1.0",
    description: Option[String] = None
)

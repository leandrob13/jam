package jam.definitions

final case class OpenAPI(openapi: String,
                         info: Info,
                         servers: Option[List[Server]],
                         paths: ???,
                         components: Option[???],
                         security: Option[List[???]],
                         tags: Option[List[???]],
                         externalDocs: Option[???])
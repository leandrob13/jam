package jam.definitions

sealed trait Parameters
private[definitions] final case class Parameter(name: String,
                                                in: String,
                                                description: Option[String],
                                                required: Boolean = false,
                                                deprecated: Boolean = false,
                                                allowEmptyValue: Boolean = false) extends Parameters

private[definitions] final case class ExternalDocumentation(description: Option[String],
                                                            url: String)
private[definitions] final case class Reference(`$ref`: String) extends Parameters

private[definitions] final case class MediaType(schema: ???,
                                                example: )

private[definitions] final case class RequestBody(description: Option[String],
                                                  content: Map[String, MediaType],
                                                  required: Boolean = false)

private[definitions] final case class Operation(tags: Option[List[String]],
                                                summary: Option[String],
                                                description: Option[String],
                                                externalDocs: Option[ExternalDocumentation],
                                                operationId: Option[String],
                                                parameters: Parameters,

                                               )

private[definitions] final case class PathItem(`$ref`: String,
                                               summary: Option[String],
                                               description: Option[String],
                                               get: Option[Operation],
                                               put: Option[Operation],
                                               post: Option[Operation],
                                               delete: Option[Operation],
                                               options: Option[Operation],
                                               head: Option[Operation],
                                               patch: Option[Operation],
                                               trace: Option[Operation],
                                               servers: Option[List[Server]],
                                               parameters: Parameters
                                              )

private[definitions] final case class Paths(path: PathItem)
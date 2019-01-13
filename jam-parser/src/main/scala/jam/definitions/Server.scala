package jam.definitions

private[definitions] final case class ServerVariable(enum: String, //TODO Enum
                                                     default: String,
                                                     description: Option[String])

private[definitions] final case class Server(url: String,
                                             description: Option[String],
                                             variables: Map[String, ServerVariable])
package jam.definitions

private[definitions] final case class Contact(name: Option[String],
                                              url: Option[String],
                                              email: Option[String])

private[definitions] final case class License(name: String,
                                              url: Option[String])

private[definitions] final case class Info(title: String,
                                           description: Option[String],
                                           termsOfService: Option[String],
                                           contact: Option[Contact],
                                           license: Option[License],
                                           version: String)
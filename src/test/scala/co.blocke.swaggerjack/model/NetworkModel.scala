package co.blocke.swaggerjack.model

import java.net.{URI, URL}
import java.util.UUID

case class NetworkModel(
                         uuid: UUID,
                         uri: URI,
                         url: URL
                       )

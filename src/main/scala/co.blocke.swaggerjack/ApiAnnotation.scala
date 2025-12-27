package co.blocke.swaggerjack

import scala.annotation.StaticAnnotation

final case class ApiDoc(
                         description: String,
                         example: String = ""
                       ) extends StaticAnnotation
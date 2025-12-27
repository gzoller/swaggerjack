package co.blocke.swaggerjack.model

import scala.util.Try

case class EitherTryUnionModel(
                                either: Either[String, Int],
                                attempt: Try[Double],
                                union: String | Int
                              )

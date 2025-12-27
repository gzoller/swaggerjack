package co.blocke.swaggerjack

import co.blocke.scala_reflection.RType
import sttp.tapir.Schema


// Usage: val journeySchema = SwaggerJack.schemaOf[Journey]
object SwaggerJack:
  inline def schemaOf[T]: Schema[T] =
    RTypeSchemaBuilder.schemaFor(RType.of[T])

/* Usage:
  import swaggerjack.auto._

  given Schema[Journey]
*/
object auto:
  inline given derived[T]: Schema[T] =
  SwaggerJack.schemaOf[T]

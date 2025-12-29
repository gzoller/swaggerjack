package co.blocke.swaggerjack

import sttp.tapir.Validator
import sttp.tapir.SchemaType

/** Internal carrier used while building schemas from RType.
  * Only converted to tapir Schema[T] at the boundary.
  */

final case class BuiltSchema(
    schemaType: SchemaType[?],
    isOptional: Boolean = false,
    name: Option[String] = None,
    description: Option[String] = None,
    format: Option[String] = None,
    validator: Validator[?] = Validator.pass,
    example: Option[Any] = None
)

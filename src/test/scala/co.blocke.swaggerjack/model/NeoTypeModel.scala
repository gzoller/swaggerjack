package co.blocke.swaggerjack.model

import neotype.*

type NonEmptyString = NonEmptyString.Type
object NonEmptyString extends Newtype[String]:
  override inline def validate(input: String): Boolean =
    input.nonEmpty

opaque type Email = String

case class NeoTypeModel(
                         name: NonEmptyString,
                         email: Email
                       )

package co.blocke.swaggerjack

import sttp.tapir.Schema

// A tiny recursive model to exercise the hard case
final case class Example(
    coords: (Int, String, java.util.UUID)
)

@main def runSwaggerJackDemo(): Unit =
  // Explicit API
  val schema1: Schema[Example] = SwaggerJack.schemaOf[Example]

  // Auto-derivation API
  import co.blocke.swaggerjack.auto.given
  val schema2: Schema[Example] = summon[Schema[Example]]

  println("Explicit schema:")
  println(schema1)

  println("\nAuto-derived schema:")
  println(schema2)

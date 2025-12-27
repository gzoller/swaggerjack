package co.blocke.swaggerjack

import sttp.tapir.Schema

// A tiny recursive model to exercise the hard case
case class Node(value: Int, next: Option[Node])

@main def runSwaggerJackDemo(): Unit =
  // Explicit API
  val schema1: Schema[Node] = SwaggerJack.schemaOf[Node]

  // Auto-derivation API
  import co.blocke.swaggerjack.auto.given
  val schema2: Schema[Node] = summon[Schema[Node]]

  println("Explicit schema:")
  println(schema1)

  println("\nAuto-derived schema:")
  println(schema2)

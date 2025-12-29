package co.blocke.swaggerjack

sealed trait ApiExample

object ApiExample:

  case object None extends ApiExample

  // Simple scalar examples (rendered as JSON literals)
  final case class Simple(value: String) extends ApiExample

  // Structured examples (rendered recursively)
  final case class Value[A](value: A) extends ApiExample

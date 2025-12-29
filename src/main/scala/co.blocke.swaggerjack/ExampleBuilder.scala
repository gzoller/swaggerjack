package co.blocke.swaggerjack

import co.blocke.scala_reflection.RType
import co.blocke.scala_reflection.rtypes.*

object ExampleBuilder:

  private def constructInstance(
      clazz: Class[?],
      args: Seq[Any]
  ): Any =
    val ctor = clazz.getConstructors.head
    ctor.newInstance(args.map(_.asInstanceOf[Object])*)

  def exampleOf(rtype: RType[?]): Option[Any] =
    exampleOf(rtype, seen = Set.empty)

  private def exampleOf(
      rtype: RType[?],
      seen: Set[RType[?]]
  ): Option[Any] =
    if seen.contains(rtype) then None
    else
      rtype match

        // ─────────────────────────────
        // Option[T] → example of T
        // ─────────────────────────────

        case o: OptionRType[?] =>
          exampleOf(o.optionParamType, seen + rtype)

        // ─────────────────────────────
        // Collections → single element
        // ─────────────────────────────
        case s: SeqRType[?] =>
          exampleOf(s.elementType, seen + rtype)
            .map(v => Seq(v))

        case a: ArrayRType[?] =>
          exampleOf(a.elementType, seen + rtype)
            .map(v => Array(v))

        // ─────────────────────────────
        // Maps → single entry
        // ─────────────────────────────

        case m: MapRType[?] =>
          for
            key <- exampleOf(m.elementType, seen + rtype)
            value <- exampleOf(m.elementType2, seen + rtype)
          yield Map(key -> value)

        // ─────────────────────────────
        // Case classes / records
        // ─────────────────────────────

        case c: ClassRType[?] =>
          val values =
            c.fields.flatMap { field =>
              exampleOf(field.fieldType, seen + rtype)
            }

          if values.size == c.fields.size then
            Some(
              constructInstance(c.clazz, values)
            )
          else None

        // ─────────────────────────────
        // Sealed traits → pick first child
        // ─────────────────────────────

        case s: TraitRType[?] if s.isSealed =>
          s.sealedChildren.headOption.flatMap { child =>
            exampleOf(child, seen + rtype)
          }

        // ─────────────────────────────
        // Everything else → no example
        // ─────────────────────────────

        case _ =>
          None

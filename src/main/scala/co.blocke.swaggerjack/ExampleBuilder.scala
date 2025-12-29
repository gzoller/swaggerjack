/*
 * Copyright (c) 2025 Greg Zoller
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

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

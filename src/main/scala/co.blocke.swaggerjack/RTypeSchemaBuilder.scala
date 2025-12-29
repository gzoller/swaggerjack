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
import co.blocke.scala_reflection.Clazzes.*
import co.blocke.scala_reflection.rtypes.*

import sttp.tapir.SchemaType.*
import sttp.tapir.{FieldName, Schema, SchemaType, Validator}

object RTypeSchemaBuilder:

  // Cache by stable id (rt.name is okay if stable/fully-qualified)
  final case class BuildContext(seen: Map[String, BuiltSchema] = Map.empty):
    def get(id: String): Option[BuiltSchema] = seen.get(id)
    def put(id: String, schema: BuiltSchema): BuildContext = copy(seen = seen + (id -> schema))

  private val ApiDocFqn = "co.blocke.swaggerjack.ApiDoc"

  // returns (description, exampleStringFromAnnotations)
  private def extractApiDocRaw(fInfo: FieldInfo): (Option[String], Option[String]) =
    fInfo.annotations
      .get(ApiDocFqn)
      .map { m =>
        (
          m.get("description").filter(_.nonEmpty),
          m.get("example").filter(_.nonEmpty)
        )
      }
      .getOrElse((None, None))

  // decode what scala-reflection stored (a stringified Scala expression) into an actual example value
  private def decodeExample(raw: String, fieldRt: RType[?]): Option[Any] =
    val s0 = raw.trim.stripPrefix("example = ").trim

    // ApiExample.None
    if s0.endsWith("ApiExample.None") || s0.endsWith("ApiExample.None$") then None
    else
      val SimplePrefix = "co.blocke.swaggerjack.ApiExample.Simple.apply(\""

      if s0.startsWith(SimplePrefix) && s0.endsWith("\")") then
        val inner = s0.substring(SimplePrefix.length, s0.length - 2)

        fieldRt match
          case _: PrimitiveRType if fieldRt.name == BOOLEAN_CLASS || fieldRt.name == JBOOLEAN_CLASS =>
            inner.toLowerCase match
              case "true"  => Some(true)
              case "false" => Some(false)
              case _       => Some(inner)

          case _: UUIDRType =>
            Some(inner) // Swagger wants UUID examples as strings

          case _: InstantRType | _: LocalDateRType | _: LocalDateTimeRType | _: OffsetDateTimeRType | _: ZonedDateTimeRType =>
            Some(inner) // date/time examples are strings

          case _ =>
            fieldRt.name match
              case INT_CLASS | JINTEGER_CLASS | LONG_CLASS | JLONG_CLASS | SHORT_CLASS | JSHORT_CLASS | BYTE_CLASS | JBYTE_CLASS =>
                inner.toLongOption
                  .orElse(inner.toIntOption)
                  .map(_.asInstanceOf[Any])
                  .orElse(Some(inner))

              case DOUBLE_CLASS | JDOUBLE_CLASS | FLOAT_CLASS | JFLOAT_CLASS | BIG_DECIMAL_CLASS | JBIG_DECIMAL_CLASS =>
                inner.toDoubleOption
                  .map(_.asInstanceOf[Any])
                  .orElse(Some(inner))

              case _ =>
                Some(inner)
      else if s0.contains("co.blocke.swaggerjack.ApiExample.Value.apply") then None // cannot deserialize structured examples from strings
      else None

  // ----------------------------
  // public entry
  // ----------------------------
  def schemaFor[T](rt: RType[T]): Schema[T] =
    toTapirSchema[T](buildSchema(rt, BuildContext()))

  // ----------------------------
  // primitives
  // ----------------------------
  private val primitiveSchemaTypes: Map[String, SchemaType[?]] =
    Map(
      BOOLEAN_CLASS -> SchemaType.SBoolean(),
      JBOOLEAN_CLASS -> SchemaType.SBoolean(),
      BYTE_CLASS -> SchemaType.SInteger(),
      SHORT_CLASS -> SchemaType.SInteger(),
      INT_CLASS -> SchemaType.SInteger(),
      LONG_CLASS -> SchemaType.SInteger(),
      JBYTE_CLASS -> SchemaType.SInteger(),
      JSHORT_CLASS -> SchemaType.SInteger(),
      JINTEGER_CLASS -> SchemaType.SInteger(),
      JLONG_CLASS -> SchemaType.SInteger(),
      FLOAT_CLASS -> SchemaType.SNumber(),
      DOUBLE_CLASS -> SchemaType.SNumber(),
      BIG_DECIMAL_CLASS -> SchemaType.SNumber(),
      BIG_INT_CLASS -> SchemaType.SInteger(),
      JFLOAT_CLASS -> SchemaType.SNumber(),
      JDOUBLE_CLASS -> SchemaType.SNumber(),
      JBIG_DECIMAL_CLASS -> SchemaType.SNumber(),
      JBIG_INTEGER_CLASS -> SchemaType.SInteger(),
      JNUMBER_CLASS -> SchemaType.SNumber(),
      STRING_CLASS -> SchemaType.SString(),
      CHAR_CLASS -> SchemaType.SString(),
      JCHARACTER_CLASS -> SchemaType.SString(),
      ANY_CLASS -> SchemaType.SString(),
      ANYVAL_CLASS -> SchemaType.SString()
    )

  // ----------------------------
  // BuiltSchema -> Tapir Schema
  // ----------------------------
  private def toTapirSchema[T](bs: BuiltSchema): Schema[T] =
    Schema[T](
      schemaType = bs.schemaType.asInstanceOf[SchemaType[T]],
      isOptional = bs.isOptional,
      description = bs.description,
      format = bs.format,
      validator = bs.validator.asInstanceOf[Validator[T]],
      encodedExample = bs.example
    ).copy(
      name = bs.name.map(n => Schema.SName(n, Nil))
    )

  // ----------------------------
  // core builder
  // ----------------------------
  private def buildSchema(rt: RType[?], ctx: BuildContext): BuiltSchema =
    val id = rt.name

    ctx.get(id) match
      case Some(existing) => existing
      case None =>
        rt match

          // ---------- primitives ----------
          case p: PrimitiveRType =>
            val st =
              primitiveSchemaTypes.getOrElse(
                p.name,
                throw new UnsupportedOperationException(s"Unsupported primitive type: ${p.name}")
              )
            BuiltSchema(
              schemaType = st,
              isOptional = p.isNullable
            )

          // ---------- class/product ----------
          case c: ClassRType[?] =>
            val typeName = c.name

            // placeholder to break recursion; IMPORTANT: give it a name too
            val placeholder =
              BuiltSchema(
                schemaType = SchemaType.SProduct[Any](Nil),
                name = Some(typeName)
              )

            val ctx2 = ctx.put(typeName, placeholder)

            val fields: List[SchemaType.SProductField[Any]] =
              c.fields.map { fInfo =>
                val base = buildSchema(fInfo.fieldType, ctx2)

                val (docDesc, docExampleRaw) = extractApiDocRaw(fInfo)

                val decodedFromAnno: Option[Any] =
                  docExampleRaw.flatMap(ex => decodeExample(ex, fInfo.fieldType))

                // If annotation was ApiExample.Value(...) we can’t reify it, so we fall back to ExampleBuilder.
                val finalExample: Option[Any] =
                  decodedFromAnno
                    .orElse(base.example)
                    .orElse(ExampleBuilder.exampleOf(fInfo.fieldType))

                val enriched =
                  base.copy(
                    description = docDesc.orElse(base.description),
                    example = finalExample
                  )

                SchemaType.SProductField[Any, Any](
                  FieldName(fInfo.name),
                  toTapirSchema[Any](enriched),
                  _ => None
                )
              }

            val completed =
              placeholder.copy(
                schemaType = SchemaType.SProduct[Any](fields)
              )

            ctx2.put(typeName, completed)
            completed

          // ---------- option ----------
          case o: OptionRType[?] =>
            buildSchema(o.optionParamType, ctx).copy(isOptional = true)

          // ---------- alias ----------
          case a: AliasRType[?] =>
            buildSchema(a.unwrappedType, ctx)

          // ---------- arrays/sequences/sets ----------
          case a: ArrayRType[?] =>
            val elem = buildSchema(a.elementType, ctx)
            BuiltSchema(
              schemaType = SchemaType.SArray[Any, Any](
                element = toTapirSchema[Any](elem)
              )(_ => Iterable.empty),
              name = None
            )

          case s: SeqRType[?] =>
            val elem = buildSchema(s.elementType, ctx)
            BuiltSchema(
              schemaType = SchemaType.SArray[Any, Any](
                element = toTapirSchema[Any](elem)
              )(_ => Iterable.empty)
            )

          case s: SetRType[?] =>
            val elem = buildSchema(s.elementType, ctx)
            BuiltSchema(
              schemaType = SchemaType.SArray[Any, Any](
                element = toTapirSchema[Any](elem)
              )(_ => Iterable.empty)
            )

          // ---------- either/union/coproduct-ish ----------
          case e: EitherRType[?] =>
            val left = buildSchema(e.leftType, ctx)
            val right = buildSchema(e.rightType, ctx)

            BuiltSchema(
              schemaType = SchemaType.SCoproduct[Any](
                subtypes = List(toTapirSchema[Any](left), toTapirSchema[Any](right)),
                discriminator = None
              )(_ => None)
            )

          case u: UnionRType[?] =>
            val left = buildSchema(u.leftType, ctx)
            val right = buildSchema(u.rightType, ctx)

            BuiltSchema(
              schemaType = SchemaType.SCoproduct[Any](
                subtypes = List(toTapirSchema[Any](left), toTapirSchema[Any](right)),
                discriminator = None
              )(_ => None)
            )

          // ---------- enum ----------
          case e: EnumRType[?] =>
            BuiltSchema(
              schemaType = SchemaType.SString(),
              validator = Validator.enumeration(e.values),
              description = Some(s"Enum: ${e.values.mkString(", ")}"),
              example = e.values.headOption
            )

          // ---------- scala/java maps ----------
          case m: MapRType[?] =>
            val value = buildSchema(m.elementType2, ctx)
            BuiltSchema(
              schemaType = SchemaType.SOpenProduct[Any, Any](
                fields = Nil,
                valueSchema = toTapirSchema[Any](value)
              )(_ => Map.empty[String, Any])
            )

          case m: JavaMapRType[?] =>
            val value = buildSchema(m.elementType2, ctx)
            BuiltSchema(
              schemaType = SchemaType.SOpenProduct[Any, Any](
                fields = Nil,
                valueSchema = toTapirSchema[Any](value)
              )(_ => Map.empty[String, Any])
            )

          // ---------- neotype ----------
          case _: NeoTypeRType[?] =>
            BuiltSchema(schemaType = SchemaType.SString())

          // ---------- UUID/URL/URI ----------
          case _: UUIDRType =>
            BuiltSchema(schemaType = SchemaType.SString(), format = Some("uuid"), description = Some("UUID"))

          case _: URIRType =>
            BuiltSchema(schemaType = SchemaType.SString(), format = Some("uri"), description = Some("URI"))

          case _: URLRType =>
            BuiltSchema(schemaType = SchemaType.SString(), format = Some("url"), description = Some("URL"))

          // ---------- recursion ----------
          case s: SelfRefRType[?] =>
            ctx.get(s.name).getOrElse {
              throw new IllegalStateException(s"Unresolved self reference for type: ${s.name}")
            }

          // ---------- java.time ----------
          case _: DurationRType =>
            BuiltSchema(schemaType = SchemaType.SString(), format = Some("duration"), description = Some("ISO-8601 duration"))
          case _: InstantRType =>
            BuiltSchema(schemaType = SchemaType.SString(), format = Some("date-time"), description = Some("Instant (UTC)"))
          case _: LocalDateRType =>
            BuiltSchema(schemaType = SchemaType.SString(), format = Some("date"), description = Some("Local date (ISO-8601)"))
          case _: LocalDateTimeRType =>
            BuiltSchema(schemaType = SchemaType.SString(), format = Some("date-time"), description = Some("Local date-time (no timezone)"))
          case _: LocalTimeRType =>
            BuiltSchema(schemaType = SchemaType.SString(), format = Some("time"), description = Some("Local time"))
          case _: MonthDayRType =>
            BuiltSchema(schemaType = SchemaType.SString(), description = Some("Month-day (e.g. --12-25)"))
          case _: OffsetDateTimeRType =>
            BuiltSchema(schemaType = SchemaType.SString(), format = Some("date-time"), description = Some("Date-time with offset"))
          case _: OffsetTimeRType =>
            BuiltSchema(schemaType = SchemaType.SString(), description = Some("Time with offset"))
          case _: PeriodRType =>
            BuiltSchema(schemaType = SchemaType.SString(), format = Some("period"), description = Some("ISO-8601 period"))
          case _: YearRType =>
            BuiltSchema(schemaType = SchemaType.SInteger(), format = Some("int32"), description = Some("Year"))
          case _: YearMonthRType =>
            BuiltSchema(schemaType = SchemaType.SString(), description = Some("Year-month (YYYY-MM)"))
          case _: ZonedDateTimeRType =>
            BuiltSchema(schemaType = SchemaType.SString(), format = Some("date-time"), description = Some("Zoned date-time"))
          case _: ZoneIdRType =>
            BuiltSchema(schemaType = SchemaType.SString(), description = Some("Zone ID (IANA)"))
          case _: ZoneOffsetRType =>
            BuiltSchema(schemaType = SchemaType.SString(), description = Some("Zone offset (+02:00)"))

          // ---------- java.lang ----------
          case _: JavaObjectRType =>
            BuiltSchema(schemaType = SchemaType.SString(), description = Some("Unstructured object"))

          // ---------- sealed trait / coproduct ----------
          case t: TraitRType[?] =>
            val typeName = t.name

            val placeholder =
              BuiltSchema(
                schemaType = SchemaType.SCoproduct[Any](Nil, None)(_ => None),
                name = Some(typeName)
              )

            val ctx2 = ctx.put(typeName, placeholder)

            val childBuilt: List[BuiltSchema] =
              t.sealedChildren.map(child => buildSchema(child, ctx2))

            val completed =
              placeholder.copy(
                schemaType = SchemaType.SCoproduct[Any](
                  subtypes = childBuilt.map(bs => toTapirSchema[Any](bs)),
                  discriminator = None
                )(_ => None)
              )

            ctx2.put(typeName, completed)
            completed

          // ---------- try ----------
          case t: TryRType[?] =>
            buildSchema(t.tryType, ctx)

          // ---------- tuple (kept as your array-of-coproduct approach) ----------
          case t: TupleRType[?] =>
            val elements: List[BuiltSchema] =
              t.typeParamValues.map(rt => buildSchema(rt, ctx))

            BuiltSchema(
              schemaType = SchemaType.SArray[Any, Any](
                element = Schema[Any](
                  schemaType = SchemaType.SCoproduct[Any](
                    subtypes = elements.map(e => toTapirSchema[Any](e)),
                    discriminator = None
                  )(_ => None)
                )
              )(_ => Iterable.empty[Any])
            )

          // ---------- singleton objects (your enum-value trick) ----------
          case o: ObjectRType[?] =>
            BuiltSchema(
              schemaType = SchemaType.SString(),
              validator = Validator.enumeration(List(o.name)),
              description = Some(s"Singleton value '${o.name}'"),
              example = Some(o.name)
            )

          case other =>
            throw new UnsupportedOperationException(
              s"""
                 |SwaggerJack cannot generate an OpenAPI schema for:
                 |  RType: ${other.getClass.getName}
                 |  Name : ${other.name}
                 |""".stripMargin
            )

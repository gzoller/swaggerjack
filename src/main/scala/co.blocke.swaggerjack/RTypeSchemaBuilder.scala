package co.blocke.swaggerjack

import co.blocke.scala_reflection.RType
import co.blocke.scala_reflection.Clazzes.*
import co.blocke.scala_reflection.rtypes.*

import sttp.tapir.SchemaType.*
import sttp.tapir.{Validator, FieldName, Schema, SchemaType}


object RTypeSchemaBuilder:

  def schemaFor[T](rt: RType[T]): Schema[T] =
    schemaForAny(rt, BuildContext()).asInstanceOf[Schema[T]]


  final case class BuildContext(
                                 seen: Map[String, Schema[?]] = Map.empty
                               ):
    def get(id: String): Option[Schema[?]] = seen.get(id)
    def put(id: String, schema: Schema[?]): BuildContext =
      copy(seen = seen + (id -> schema))

  private val primitiveSchemaTypes: Map[String, SchemaType[?]] =
    Map(
      // ----- Booleans -----
      BOOLEAN_CLASS -> SchemaType.SBoolean(),
      JBOOLEAN_CLASS -> SchemaType.SBoolean(),

      // ----- Integers -----
      BYTE_CLASS -> SchemaType.SInteger(),
      SHORT_CLASS -> SchemaType.SInteger(),
      INT_CLASS -> SchemaType.SInteger(),
      LONG_CLASS -> SchemaType.SInteger(),

      JBYTE_CLASS -> SchemaType.SInteger(),
      JSHORT_CLASS -> SchemaType.SInteger(),
      JINTEGER_CLASS -> SchemaType.SInteger(),
      JLONG_CLASS -> SchemaType.SInteger(),

      // ----- Floating / decimal -----
      FLOAT_CLASS -> SchemaType.SNumber(),
      DOUBLE_CLASS -> SchemaType.SNumber(),
      BIG_DECIMAL_CLASS -> SchemaType.SNumber(),
      BIG_INT_CLASS -> SchemaType.SInteger(),

      JFLOAT_CLASS -> SchemaType.SNumber(),
      JDOUBLE_CLASS -> SchemaType.SNumber(),
      JBIG_DECIMAL_CLASS -> SchemaType.SNumber(),
      JBIG_INTEGER_CLASS -> SchemaType.SInteger(),
      JNUMBER_CLASS -> SchemaType.SNumber(),

      // ----- Strings / chars -----
      STRING_CLASS -> SchemaType.SString(),
      CHAR_CLASS -> SchemaType.SString(),
      JCHARACTER_CLASS -> SchemaType.SString(),

      // ----- Catch-alls -----
      ANY_CLASS -> SchemaType.SString(),
      ANYVAL_CLASS -> SchemaType.SString()
    )

  private def schemaForAny(
                            rt: RType[?],
                            ctx: BuildContext
                          ): Schema[?] = {

    val id = rt.name  // or stable id

    ctx.get(id) match
      case Some(existing) =>
        // Reuse the schema instance directly
        existing

      case None =>
        rt match {

          case p: PrimitiveRType =>
            val st =
              primitiveSchemaTypes.getOrElse(
                p.name,
                throw new UnsupportedOperationException(
                  s"Unsupported primitive type: ${p.name}"
                )
              )

            Schema(
              schemaType = st,
              isOptional = p.isNullable
            )

          case c: ClassRType[_] =>
            val id = c.name

            // placeholder to break recursion
            val placeholder: Schema[Any] =
              Schema(schemaType = SchemaType.SProduct[Any](Nil))

            val ctx2 = ctx.put(id, placeholder)

            val fields: List[SchemaType.SProductField[Any]] =
              c.fields.map { fInfo =>
                val doc = fInfo.annotations.get("co.blocke.swaggerjack.ApiDoc")

                val description = doc.flatMap(_.get("description"))
                val example = doc.flatMap(_.get("example")).filter(_.nonEmpty)
                val raw: Schema[?] = schemaForAny(fInfo.fieldType, ctx2)

                raw match
                  case s: Schema[f] =>
                    SchemaType.SProductField[Any, f](
                      FieldName(fInfo.name),
                      s.copy(
                        description = description,
                        encodedExample = example
                      ),
                      (_: Any) => None.asInstanceOf[Option[f]]
                    )
              }

            val completed: Schema[Any] =
              placeholder.copy(
                schemaType = SchemaType.SProduct[Any](fields)
              )

            ctx2.put(id, completed)
            completed

          case o: OptionRType[_] =>
            val s = schemaForAny(o.optionParamType, ctx)
            s.copy(isOptional = true)

          case a: AliasRType[_] =>
            schemaForAny(a.unwrappedType, ctx)

          case a: ArrayRType[_] =>
            val elemSchema = schemaForAny(a.elementType, ctx)

            Schema(
              SchemaType.SArray[Any, Any](elemSchema.asInstanceOf[Schema[Any]])(
                (_: Any) => Iterable.empty
              )
            )

          case e: EitherRType[_] =>
            val leftSchema  = schemaForAny(e.leftType, ctx)
            val rightSchema = schemaForAny(e.rightType, ctx)

            Schema(
              SchemaType.SCoproduct[Any](
                List(leftSchema, rightSchema),
                None
              )(
                (_: Any) => None
              )
            )

          case e: EnumRType[_] =>
            Schema[String](
              schemaType = SchemaType.SString(),
              validator  = Validator.enumeration(e.values)
            )

          case m: JavaMapRType[_] =>
            val valueSchema = schemaForAny(m.elementType2, ctx)

            Schema(
              SchemaType.SOpenProduct[Any, Any](
                fields = Nil,
                valueSchema = valueSchema.asInstanceOf[Schema[Any]]
              )(
                (_: Any) => Map.empty[String, Any]
              )
            )

          case m: MapRType[_] =>
            val valueSchema = schemaForAny(m.elementType2, ctx)

            Schema(
              SchemaType.SOpenProduct[Any, Any](
                fields = Nil,
                valueSchema = valueSchema.asInstanceOf[Schema[Any]]
              )(
                (_: Any) => Map.empty[String, Any]
              )
            )

          case _: NeoTypeRType[_] =>
            Schema(
              SchemaType.SString()
            )

          case _: UUIDRType =>
            Schema[String](
              schemaType  = SchemaType.SString(),
              format      = Some("uuid"),
              description = Some("UUID")
            )

          case _: URIRType =>
            Schema[String](
              schemaType  = SchemaType.SString(),
              format      = Some("uri"),
              description = Some("URI")
            )

          case _: URLRType =>
            Schema[String](
              schemaType  = SchemaType.SString(),
              format      = Some("url"),
              description = Some("URL")
            )

          case s: SelfRefRType[_] =>
            ctx.get(s.name).getOrElse {
              throw new IllegalStateException(
                s"Unresolved self reference for type: ${s.name}"
              )
            }

          case s: SeqRType[_] =>
            val elemSchema = schemaForAny(s.elementType, ctx)

            Schema[Any](
              schemaType = SchemaType.SArray[Any, Any](
                element = elemSchema.asInstanceOf[Schema[Any]]
              )(
                (_: Any) => Iterable.empty[Any]
              )
            )

          case s: SetRType[_] =>
            val elemSchema = schemaForAny(s.elementType, ctx)

            Schema[Any](
              schemaType = SchemaType.SArray[Any, Any](
                element = elemSchema.asInstanceOf[Schema[Any]]
              )(
                (_: Any) => Iterable.empty[Any]
              )
            )

          // ---------- java.time ----------
          case _: DurationRType =>
            Schema[String](
              schemaType  = SchemaType.SString(),
              format      = Some("duration"),
              description = Some("ISO-8601 duration")
            )

          case _: InstantRType =>
            Schema[String](
              schemaType  = SchemaType.SString(),
              format      = Some("date-time"),
              description = Some("Instant (UTC)")
            )

          case _: LocalDateRType =>
            Schema[String](
              schemaType  = SchemaType.SString(),
              format      = Some("date"),
              description = Some("Local date (ISO-8601)")
            )

          case _: LocalDateTimeRType =>
            Schema[String](
              schemaType  = SchemaType.SString(),
              format      = Some("date-time"),
              description = Some("Local date-time (no timezone)")
            )

          case _: LocalTimeRType =>
            Schema[String](
              schemaType  = SchemaType.SString(),
              format      = Some("time"),
              description = Some("Local time")
            )

          case _: MonthDayRType =>
            Schema[String](
              schemaType  = SchemaType.SString(),
              description = Some("Month-day (e.g. --12-25)")
            )

          case _: OffsetDateTimeRType =>
            Schema[String](
              schemaType  = SchemaType.SString(),
              format      = Some("date-time"),
              description = Some("Date-time with offset")
            )

          case _: OffsetTimeRType =>
            Schema[String](
              schemaType  = SchemaType.SString(),
              description = Some("Time with offset")
            )

          case _: PeriodRType =>
            Schema[String](
              schemaType  = SchemaType.SString(),
              format      = Some("period"),
              description = Some("ISO-8601 period")
            )

          case _: YearRType =>
            Schema[Int](
              schemaType  = SchemaType.SInteger(),
              format      = Some("int32"),
              description = Some("Year")
            )

          case _: YearMonthRType =>
            Schema[String](
              schemaType  = SchemaType.SString(),
              description = Some("Year-month (YYYY-MM)")
            )

          case _: ZonedDateTimeRType =>
            Schema[String](
              schemaType  = SchemaType.SString(),
              format      = Some("date-time"),
              description = Some("Zoned date-time")
            )

          case _: ZoneIdRType =>
            Schema[String](
              schemaType  = SchemaType.SString(),
              description = Some("Zone ID (IANA)")
            )

          case _: ZoneOffsetRType =>
            Schema[String](
              schemaType  = SchemaType.SString(),
              description = Some("Zone offset (+02:00)")
            )

          // ---------- java.lang ----------
          case _: JavaObjectRType =>
            Schema[Any](
              schemaType  = SchemaType.SString(),
              description = Some("Unstructured object")
            )

          case t: TraitRType[_] if !t.isSealed =>
            throw new IllegalArgumentException(
              s"Non-sealed trait '${t.name}' cannot be represented in OpenAPI schema"
            )

          case t: TraitRType[_] =>
            val id = t.name

            // 1️⃣ placeholder to break recursion
            val placeholder: Schema[Any] =
              Schema(schemaType = SchemaType.SCoproduct[Any](Nil, None)(_ => None))

            // 2️⃣ register early
            val ctx2 = ctx.put(id, placeholder)

            // 3️⃣ fully build children (annotation-safe)
            val childSchemas: List[Schema[Any]] =
              t.sealedChildren.map { child =>
                schemaForAny(child, ctx2).asInstanceOf[Schema[Any]]
              }

            // 4️⃣ finalize
            val completed: Schema[Any] =
              placeholder.copy(
                schemaType =
                  SchemaType.SCoproduct[Any](
                    subtypes = childSchemas,
                    discriminator = None
                  )(
                    (_: Any) => None
                  )
              )

            ctx2.put(id, completed)
            completed

          case t: TryRType[_] =>
            // Try[T] is modeled as just T at the schema level
            // (failure is an execution concern, not a data shape concern)
            schemaForAny(t.tryType, ctx)

          case t: TupleRType[_] =>
            // Tuples are represented as fixed-position arrays
            val elemSchemas: List[Schema[Any]] =
              t.typeParamValues.map { rt =>
                schemaForAny(rt, ctx).asInstanceOf[Schema[Any]]
              }

            Schema[Any](
              schemaType =
                SchemaType.SArray[Any, Any](
                  element = Schema[Any](
                    schemaType = SchemaType.SCoproduct[Any](
                      subtypes = elemSchemas,
                      discriminator = None
                    )(
                      (_: Any) => None
                    )
                  )
                )(
                  (_: Any) => Iterable.empty[Any]
                )
            )

          case u: UnionRType[_] =>
            val leftSchema  = schemaForAny(u.leftType, ctx).asInstanceOf[Schema[Any]]
            val rightSchema = schemaForAny(u.rightType, ctx).asInstanceOf[Schema[Any]]

            Schema[Any](
              schemaType =
                SchemaType.SCoproduct[Any](
                  subtypes = List(leftSchema, rightSchema),
                  discriminator = None
                )(
                  (_: Any) => None
                )
            )

          case _: ObjectRType[_] =>
            Schema[Any](
              schemaType = SchemaType.SProduct[Any](Nil)
            )

          case rt =>
            throw new UnsupportedOperationException(
              s"""
                 |SwaggerJack cannot generate an OpenAPI schema for:
                 |  RType: ${rt.getClass.getName}
                 |  Name : ${rt.name}
                 |
                 |This type is either:
                 |  • abstract
                 |  • higher-kinded
                 |  • path-dependent
                 |  • or not representable in OpenAPI
                 |""".stripMargin
            )
        }
  }


/*
swaggerjack/
  src/test/scala/
    schemas/
      PrimitiveSchemaSpec.scala
      CollectionSchemaSpec.scala
      ProductSchemaSpec.scala
      CoproductSchemaSpec.scala
      TimeSchemaSpec.scala
      EdgeCaseSchemaSpec.scala
  
def schemaOf[T]: Schema[T] =
  SwaggerJack.schemaOf[T]

def assertSchema[T](
  expectedType: SchemaType[_],
  description: Option[String] = None
)(using m: scala.reflect.ClassTag[T]): Unit = {
  val s = schemaOf[T]
  assert(s.schemaType == expectedType)
  assert(s.description == description)
}
  
OpenAPIGenerator.fromEndpoints(
  List(endpoint.out(jsonBody[Journey]))
)
*/
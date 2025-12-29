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

import munit.FunSuite
import model.*
import co.blocke.scala_reflection.RType

import sttp.tapir.Schema

import scala.util.Try

class SchemaGenerationSpec extends FunSuite {

  private def dumpSchema[T](name: String)(schema: => sttp.tapir.Schema[T]): Unit = {
    val s = schema
    println(s"\n===== $name =====")
    println(s)
  }

  private inline def schemaOf[T]: Schema[T] =
    SwaggerJack.schemaOf[T]

  // ---------- positive cases ----------

  test("PrimitiveModel schema generates") {
    val s = schemaOf[PrimitiveModel]
    assert(
      s.toString ==
        "Schema(SProduct(List(SProductField(FieldName(b,b),Schema(SBoolean(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(i,i),Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(l,l),Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(d,d),Schema(SNumber(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(bd,bd),Schema(SNumber(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(bi,bi),Schema(SInteger(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(s,s),Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(c,c),Schema(SString(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(jb,jb),Schema(SBoolean(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(ji,ji),Schema(SInteger(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(jl,jl),Schema(SInteger(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(jd,jd),Schema(SNumber(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(any,any),Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(anyVal,anyVal),Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(alias,alias),Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))))),Some(SName(co.blocke.swaggerjack.model.PrimitiveModel,List())),false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))"
    )
  }

  test("CollectionModel schema generates") {
    val s = schemaOf[CollectionModel]
    assert(
      s.toString ==
        "Schema(SProduct(List(SProductField(FieldName(listInts,listInts),Schema(SArray(Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(setStrings,setStrings),Schema(SArray(Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(seqDoubles,seqDoubles),Schema(SArray(Schema(SNumber(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(arrayLongs,arrayLongs),Schema(SArray(Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(optionalString,optionalString),Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(optionalInt,optionalInt),Schema(SInteger(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))))),Some(SName(co.blocke.swaggerjack.model.CollectionModel,List())),false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))"
    )
  }

  test("MapModel schema generates") {
    val s = schemaOf[MapModel]
    assert(
      s.toString ==
        "Schema(SProduct(List(SProductField(FieldName(scalaMap,scalaMap),Schema(SOpenProduct(List(),Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(javaMap,javaMap),Schema(SOpenProduct(List(),Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))))),Some(SName(co.blocke.swaggerjack.model.MapModel,List())),false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))"
    )
  }

  test("EnumModel schema generates") {
    val s = schemaOf[EnumModel]
    assert(
      s.toString ==
        "Schema(SProduct(List(SProductField(FieldName(status,status),Schema(SString(),None,false,Some(Enum: Pending, Active, Disabled),None,None,Some(Pending),false,false,Enumeration(List(Pending, Active, Disabled),None,None),AttributeMap(Map()))))),Some(SName(co.blocke.swaggerjack.model.EnumModel,List())),false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))"
    )
  }

  test("TimeModel schema generates") {
    val s = schemaOf[TimeModel]
    assert(
      s.toString ==
        "Schema(SProduct(List(SProductField(FieldName(instant,instant),Schema(SString(),None,false,Some(Instant (UTC)),None,Some(date-time),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(localDate,localDate),Schema(SString(),None,false,Some(Local date (ISO-8601)),None,Some(date),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(localDateTime,localDateTime),Schema(SString(),None,false,Some(Local date-time (no timezone)),None,Some(date-time),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(localTime,localTime),Schema(SString(),None,false,Some(Local time),None,Some(time),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(offsetDateTime,offsetDateTime),Schema(SString(),None,false,Some(Date-time with offset),None,Some(date-time),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(zonedDateTime,zonedDateTime),Schema(SString(),None,false,Some(Zoned date-time),None,Some(date-time),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(duration,duration),Schema(SString(),None,false,Some(ISO-8601 duration),None,Some(duration),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(period,period),Schema(SString(),None,false,Some(ISO-8601 period),None,Some(period),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(year,year),Schema(SInteger(),None,false,Some(Year),None,Some(int32),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(yearMonth,yearMonth),Schema(SString(),None,false,Some(Year-month (YYYY-MM)),None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(zoneId,zoneId),Schema(SString(),None,false,Some(Zone ID (IANA)),None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(zoneOffset,zoneOffset),Schema(SString(),None,false,Some(Zone offset (+02:00)),None,None,None,false,false,All(List()),AttributeMap(Map()))))),Some(SName(co.blocke.swaggerjack.model.TimeModel,List())),false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))"
    )
  }

  test("Recursive Node schema generates") {
    val s = schemaOf[Node]
    assert(
      s.toString ==
        "Schema(SProduct(List(SProductField(FieldName(value,value),Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(next,next),Schema(SProduct(List()),Some(SName(co.blocke.swaggerjack.model.Node,List())),true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))))),Some(SName(co.blocke.swaggerjack.model.Node,List())),false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))"
    )
  }

  test("Sealed trait Shape generates coproduct field") {
    val s = schemaOf[Drawing]
    assert(
      s.toString ==
        "Schema(SProduct(List(SProductField(FieldName(shape,shape),Schema(SCoproduct(List(Schema(SProduct(List(SProductField(FieldName(radius,radius),Schema(SNumber(),None,false,Some(Radius of the circle),None,None,None,false,false,All(List()),AttributeMap(Map()))))),Some(SName(co.blocke.swaggerjack.model.Circle,List())),false,None,None,None,None,false,false,All(List()),AttributeMap(Map())), Schema(SProduct(List(SProductField(FieldName(width,width),Schema(SNumber(),None,false,Some(Width of the rectangle),None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(height,height),Schema(SNumber(),None,false,Some(Height of the rectangle),None,None,None,false,false,All(List()),AttributeMap(Map()))))),Some(SName(co.blocke.swaggerjack.model.Rectangle,List())),false,None,None,None,None,false,false,All(List()),AttributeMap(Map())), Schema(SString(),None,false,Some(Singleton value 'co.blocke.swaggerjack.model.Origin'),None,None,Some(co.blocke.swaggerjack.model.Origin),false,false,Enumeration(List(co.blocke.swaggerjack.model.Origin),None,None),AttributeMap(Map()))),None),Some(SName(co.blocke.swaggerjack.model.Shape,List())),false,Some(The geometric shape being drawn),None,None,None,false,false,All(List()),AttributeMap(Map()))))),Some(SName(co.blocke.swaggerjack.model.Drawing,List())),false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))"
    )
  }

  test("Either / Try / Union schema generates") {
    val s = schemaOf[EitherTryUnionModel]
    assert(
      s.toString ==
        "Schema(SProduct(List(SProductField(FieldName(either,either),Schema(SCoproduct(List(Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map())), Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(attempt,attempt),Schema(SNumber(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(union,union),Schema(SCoproduct(List(Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map())), Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))))),Some(SName(co.blocke.swaggerjack.model.EitherTryUnionModel,List())),false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))"
    )
  }

  test("Tuple schema generates") {
    val s = schemaOf[TupleModel]
    assert(
      s.toString ==
        "Schema(SProduct(List(SProductField(FieldName(pair,pair),Schema(SArray(Schema(SCoproduct(List(Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map())), Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(triple,triple),Schema(SArray(Schema(SCoproduct(List(Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map())), Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map())), Schema(SBoolean(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))))),Some(SName(co.blocke.swaggerjack.model.TupleModel,List())),false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))"
    )
  }

  test("NeoType schema generates") {
    val s = schemaOf[NeoTypeModel]
    assert(
      s.toString ==
        "Schema(SProduct(List(SProductField(FieldName(name,name),Schema(SString(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(email,email),Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))))),Some(SName(co.blocke.swaggerjack.model.NeoTypeModel,List())),false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))"
    )
  }

  test("Network types schema generates") {
    val s = schemaOf[NetworkModel]
    assert(
      s.toString ==
        "Schema(SProduct(List(SProductField(FieldName(uuid,uuid),Schema(SString(),None,false,Some(UUID),None,Some(uuid),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(uri,uri),Schema(SString(),None,false,Some(URI),None,Some(uri),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(url,url),Schema(SString(),None,false,Some(URL),None,Some(url),None,false,false,All(List()),AttributeMap(Map()))))),Some(SName(co.blocke.swaggerjack.model.NetworkModel,List())),false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))"
    )
  }
}

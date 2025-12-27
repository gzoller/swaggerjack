package co.blocke.swaggerjack

import munit.FunSuite
import model.*
import co.blocke.scala_reflection.RType

import sttp.tapir.Schema

import scala.util.Try

class SchemaGenerationSpec extends FunSuite {

  private inline def schemaOf[T]: Schema[T] =
    SwaggerJack.schemaOf[T]

  // ---------- positive cases ----------

  test("PrimitiveModel schema generates") {
    val s = schemaOf[PrimitiveModel]
    assert(s.toString == """Schema(SProduct(List(SProductField(FieldName(b,b),Schema(SBoolean(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(i,i),Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(l,l),Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(d,d),Schema(SNumber(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(bd,bd),Schema(SNumber(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(bi,bi),Schema(SInteger(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(s,s),Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(c,c),Schema(SString(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(jb,jb),Schema(SBoolean(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(ji,ji),Schema(SInteger(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(jl,jl),Schema(SInteger(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(jd,jd),Schema(SNumber(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(any,any),Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(anyVal,anyVal),Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(alias,alias),Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))""")
  }

  test("CollectionModel schema generates") {
    val s = schemaOf[CollectionModel]
    assert(s.toString == """Schema(SProduct(List(SProductField(FieldName(listInts,listInts),Schema(SArray(Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(setStrings,setStrings),Schema(SArray(Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(seqDoubles,seqDoubles),Schema(SArray(Schema(SNumber(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(arrayLongs,arrayLongs),Schema(SArray(Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(optionalString,optionalString),Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(optionalInt,optionalInt),Schema(SInteger(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))""")
  }

  test("MapModel schema generates") {
    val s = schemaOf[MapModel]
    assert(s.toString == """Schema(SProduct(List(SProductField(FieldName(scalaMap,scalaMap),Schema(SOpenProduct(List(),Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(javaMap,javaMap),Schema(SOpenProduct(List(),Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))""")
  }

  test("EnumModel schema generates") {
    val s = schemaOf[EnumModel]
    assert(s.toString == """Schema(SProduct(List(SProductField(FieldName(status,status),Schema(SString(),None,false,None,None,None,None,false,false,Enumeration(List(Pending, Active, Disabled),None,None),AttributeMap(Map()))))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))""")
  }

  test("TimeModel schema generates") {
    val s = schemaOf[TimeModel]
    assert(s.toString == """Schema(SProduct(List(SProductField(FieldName(instant,instant),Schema(SString(),None,false,None,None,Some(date-time),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(localDate,localDate),Schema(SString(),None,false,None,None,Some(date),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(localDateTime,localDateTime),Schema(SString(),None,false,None,None,Some(date-time),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(localTime,localTime),Schema(SString(),None,false,None,None,Some(time),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(offsetDateTime,offsetDateTime),Schema(SString(),None,false,None,None,Some(date-time),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(zonedDateTime,zonedDateTime),Schema(SString(),None,false,None,None,Some(date-time),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(duration,duration),Schema(SString(),None,false,None,None,Some(duration),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(period,period),Schema(SString(),None,false,None,None,Some(period),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(year,year),Schema(SInteger(),None,false,None,None,Some(int32),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(yearMonth,yearMonth),Schema(SString(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(zoneId,zoneId),Schema(SString(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(zoneOffset,zoneOffset),Schema(SString(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))""")
  }

  test("Recursive Node schema generates") {
    val s = schemaOf[Node]
    assert(s.toString == """Schema(SProduct(List(SProductField(FieldName(value,value),Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(next,next),Schema(SProduct(List()),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))""")
  }

  test("Sealed trait Shape generates coproduct field") {
    val s = schemaOf[Drawing]
    assert(s.toString == """Schema(SProduct(List(SProductField(FieldName(shape,shape),Schema(SCoproduct(List(Schema(SProduct(List(SProductField(FieldName(radius,radius),Schema(SNumber(),None,false,Some(Radius of the circle),None,None,Some(5.0),false,false,All(List()),AttributeMap(Map()))))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map())), Schema(SProduct(List(SProductField(FieldName(width,width),Schema(SNumber(),None,false,Some(Width of the rectangle),None,None,Some(10.0),false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(height,height),Schema(SNumber(),None,false,Some(Height of the rectangle),None,None,Some(4.5),false,false,All(List()),AttributeMap(Map()))))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map())), Schema(SProduct(List()),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None),None,false,Some(The geometric shape being drawn),None,None,Some(co.blocke.swaggerjack.ApiDoc.$lessinit$greater$default$2),false,false,All(List()),AttributeMap(Map()))))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))""")
  }

  test("Either / Try / Union schema generates") {
    val s = schemaOf[EitherTryUnionModel]
    assert(s.toString == """Schema(SProduct(List(SProductField(FieldName(either,either),Schema(SCoproduct(List(Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map())), Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(attempt,attempt),Schema(SNumber(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(union,union),Schema(SCoproduct(List(Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map())), Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))""")
  }

  test("Tuple schema generates") {
    val s = schemaOf[TupleModel]
    assert(s.toString == """Schema(SProduct(List(SProductField(FieldName(pair,pair),Schema(SArray(Schema(SCoproduct(List(Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map())), Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(triple,triple),Schema(SArray(Schema(SCoproduct(List(Schema(SInteger(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map())), Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map())), Schema(SBoolean(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))""")
  }

  test("NeoType schema generates") {
    val s = schemaOf[NeoTypeModel]
    assert(s.toString == """Schema(SProduct(List(SProductField(FieldName(name,name),Schema(SString(),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(email,email),Schema(SString(),None,true,None,None,None,None,false,false,All(List()),AttributeMap(Map()))))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))""")
  }

  test("Network types schema generates") {
    val s = schemaOf[NetworkModel]
    assert(s.toString == """Schema(SProduct(List(SProductField(FieldName(uuid,uuid),Schema(SString(),None,false,None,None,Some(uuid),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(uri,uri),Schema(SString(),None,false,None,None,Some(uri),None,false,false,All(List()),AttributeMap(Map()))), SProductField(FieldName(url,url),Schema(SString(),None,false,None,None,Some(url),None,false,false,All(List()),AttributeMap(Map()))))),None,false,None,None,None,None,false,false,All(List()),AttributeMap(Map()))""")
  }
}

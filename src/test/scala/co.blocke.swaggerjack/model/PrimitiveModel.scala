package co.blocke.swaggerjack.model

type UserId = String

case class PrimitiveModel(
                           b: Boolean,
                           i: Int,
                           l: Long,
                           d: Double,
                           bd: BigDecimal,
                           bi: BigInt,
                           s: String,
                           c: Char,

                           jb: java.lang.Boolean,
                           ji: java.lang.Integer,
                           jl: java.lang.Long,
                           jd: java.lang.Double,

                           any: Any,
                           anyVal: AnyVal,

                           alias: UserId
                         )

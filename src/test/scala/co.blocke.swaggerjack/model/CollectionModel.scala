package co.blocke.swaggerjack.model

case class CollectionModel(
    listInts: List[Int],
    setStrings: Set[String],
    seqDoubles: Seq[Double],
    arrayLongs: Array[Long],
    optionalString: Option[String],
    optionalInt: Option[Int]
)

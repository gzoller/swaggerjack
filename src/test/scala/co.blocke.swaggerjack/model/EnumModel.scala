package co.blocke.swaggerjack.model

enum Status:
  case Pending, Active, Disabled

case class EnumModel(
    status: Status
)

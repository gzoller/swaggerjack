package co.blocke.swaggerjack.model

import java.time.*

case class TimeModel(
                      instant: Instant,
                      localDate: LocalDate,
                      localDateTime: LocalDateTime,
                      localTime: LocalTime,
                      offsetDateTime: OffsetDateTime,
                      zonedDateTime: ZonedDateTime,
                      duration: Duration,
                      period: Period,
                      year: Year,
                      yearMonth: YearMonth,
                      zoneId: ZoneId,
                      zoneOffset: ZoneOffset
                    )

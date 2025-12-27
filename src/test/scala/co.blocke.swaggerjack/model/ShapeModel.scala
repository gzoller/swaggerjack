package co.blocke.swaggerjack
package model

sealed trait Shape

final case class Circle(
                         @ApiDoc(
                           description = "Radius of the circle",
                           example = "5.0"
                         )
                         radius: Double
                       ) extends Shape

final case class Rectangle(
                            @ApiDoc(
                              description = "Width of the rectangle",
                              example = "10.0"
                            )
                            width: Double,

                            @ApiDoc(
                              description = "Height of the rectangle",
                              example = "4.5"
                            )
                            height: Double
                          ) extends Shape

case object Origin extends Shape


final case class Drawing(
                          @ApiDoc(
                            description = "The geometric shape being drawn"
                          )
                          shape: Shape
                        )

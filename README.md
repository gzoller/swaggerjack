# swaggerjack
Library that uses scala-reflection to power OpenAPI doc generation in Tapir for those only interested in doc generation, not hosted sample pages. (In other
words, those who don't want to use Tapir as their framework.)

You annotate classes like this:

```scala
import co.blocke.swaggerjack.ApiExample

sealed trait Shape

final case class Circle(
                         @ApiDoc(
                           description = "Radius of the circle",
                           example = ApiExample.Value(5.0)
                         )
                         radius: Double
                       ) extends Shape

final case class Rectangle(
                            @ApiDoc(
                              description = "Width of the rectangle",
                              example = ApiExample.Value(10.0)
                            )
                            width: Double,

                            @ApiDoc(
                              description = "Height of the rectangle",
                              example = ApiExample.Value(4.5)
                            )
                            height: Double
                          ) extends Shape

case object Origin extends Shape

final case class Drawing(
                          @ApiDoc(
                            description = "The geometric shape being drawn",
                            example = ApiExample.Value(Circle(3.0))
                          )
                          shape: Shape
                        )
```

You can see here that you can provide examples that are simple values or nested classes like Circle, shown here.

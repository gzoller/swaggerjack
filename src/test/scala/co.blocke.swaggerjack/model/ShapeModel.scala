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
package model

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

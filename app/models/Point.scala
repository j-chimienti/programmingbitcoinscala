package models

case class Point(x: Option[BigInt] = None,
                 y: Option[BigInt] = None,
                 a: BigInt,
                 b: BigInt) {

  if (x.isDefined && y.isDefined)
    if (y.get.pow(2) != x.get.pow(3) + (a * x.get) + b)
      throw new RuntimeException(s"($x, $y) is not on the curve")

  override def toString: String =
    if (x.isEmpty) s"Point(infinity)" else s"Point($x, $y, $a, $b)"
  def equals(obj: Point): Boolean =
    x == obj.x && y == obj.y && a == obj.a && b == obj.b
  def same(point: Point): Boolean = this == point && y.get == 0
  def isAtInfinity(x: Option[BigInt]): Boolean = x.isEmpty
  def requireSameCurve(point: Point): Unit =
    if (a != point.a || b != point.b)
      throw new RuntimeException(
        s"Points ${this}, $point are not on the same curve"
      )

  def !=(point: Point): Boolean = {
    x != point.x || y != point.y || a != point.a || b != point.b
  }

  def *(coef: BigInt): Point = {
    this.*(coef.toInt)
  }

  def *(coeff: Int): Point = {
    var product = Point(a = a, b = b)
    for (_ <- 1 to coeff) product += this
    product
  }

  @throws(classOf[RuntimeException])
  def +(that: Point): Point = {
    requireSameCurve(that)
    if (this == that && y.get == 0)
      Point(a = a, b = b)
    else if (x.isEmpty) that
    else if (that.x.isEmpty) this
    else if (x == that.x && y != that.y) Point(a = a, b = b)
    else if (x != that.x) {
      val slope = (that.y.get - y.get) / (that.x.get - x.get)
      val x3 = slope.pow(2) - x.get - that.x.get
      val y3 = slope * (x.get - x3) - y.get
      Point(Some(x3), Some(y3), a, b)
    } else {
      val s = (3 * x.get.pow(2) + a) / (2 * y.get)
      val xx = s.pow(2) - 2 * x.get
      val yy = s * (x.get - xx) - y.get
      Point(Some(xx), Some(yy), a, b)
    }
  }

}

package models

trait PointNum {

  if (x.isDefined && y.isDefined) {
    if (y.get.pow(2) != x.get.pow(3) + (a * x.get) + b) {
      throw new RuntimeException(s"($x, $y) is not on the curve")
    }
  }
  override def toString: String =
    if (x.isEmpty) s"Point(infinity)" else s"Point($x, $y, $a, $b)"
  def x: Option[BigInt]
  def y: Option[BigInt]
  def a: BigInt
  def b: BigInt
  def equals(obj: Point): Boolean =
    x == obj.x && y == obj.y && a == obj.a && b == obj.b
  def same(point: Point): Boolean = this == point && y.get == 0
  def isAtInfinity(x: Option[BigInt]): Boolean = x.isEmpty
  def requireSameCurve(point: Point): Unit =
    if (a != point.a || b != point.b)
      throw new RuntimeException(
        s"Points ${this}, $point are not on the same curve"
      )
}

case class Point(x: Option[BigInt] = None,
                 y: Option[BigInt] = None,
                 a: BigInt,
                 b: BigInt)
    extends PointNum {

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
  def +(point: Point): Point = {
    if (a != point.a || b != point.b) {
      throw new RuntimeException(
        s"Points $this, $point are not on the same curve"
      )
    }
    if (this == point && y.get == 0) {
      Point(a = a, b = b)
      // Case 0.0: this is the point at infinity, return other point
    } else if (x.isEmpty) {
      point
      // Case 0.0: other is the point at infinity, return this point
    } else if (point.x.isEmpty) {
      this
      // Case 1: self.x == other.x, self.y != other.y
    } else if (x == point.x && y != point.y) {
      Point(a = a, b = b)
      //      # Case 2: self.x != other.x
    } else if (x != point.x) {
      val slope = (point.y.get - y.get) / (point.x.get - x.get)
      val x3 = slope.pow(2) - x.get - point.x.get
      val y3 = slope * (x.get - x3) - y.get
      Point(Some(x3), Some(y3), a, b)
      // # Case 3: self.x == other.x, self.y == other.y
    } else {
      val s = (3 * x.get.pow(2) + a) / (2 * y.get)
      val xx = s.pow(2) - 2 * x.get
      val yy = s * (x.get - xx) - y.get
      Point(Some(xx), Some(yy), a, b)
    }
  }

}

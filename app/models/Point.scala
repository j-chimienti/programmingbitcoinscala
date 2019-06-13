package models

case class PointFE(x: Option[FieldElement] = None,
                   y: Option[FieldElement] = None,
                   a: FieldElement,
                   b: FieldElement) {

  if (x.isDefined && y.isDefined)
    if (y.get ** 2 != x.get ** 3 + (a * x.get) + b)
      throw new RuntimeException(s"(${x.get}, ${y.get}) is not on the curve")

  def +(that: PointFE): PointFE = {
    requireSameCurve(that)
    if (this == that && y.get.num.get == 0) PointFE(None, None, a, b)
    else if (x.isEmpty) that
    else if (that.x.isEmpty) PointFE(x, y, a, b)
    else if (x == that.x && y != that.y)
      PointFE(None, None, a, b)
    else if (x != that.x) {
      val denom: FieldElement = that.x.get - x.get
      val slope: FieldElement = (that.y.get - y.get) / denom
      val x3 = slope ** 2 - x.get - that.x.get
      val y3 = slope * (x.get - x3) - y.get
      PointFE(Some(x3), Some(y3), a, b)
    } else {
      val numerator = x.get ** 2 + a
      val s = (numerator * 3) / (y.get * 2)
      val xx = s ** 2 - x.get * 2
      val yy = s * (x.get - xx) - y.get
      PointFE(Some(xx), Some(yy), a, b)
    }
  }

  def *(coeff: Int): PointFE = {
    var product = PointFE(None, None, a = a, b = b)
    for (_ <- 1 to coeff) product += this
    product
  }

  def same(point: PointFE): Boolean = this == point && y.get.num.get == 0

  override def toString: String =
    if (x.isEmpty) "Point(infinity)"
    else s"Point(${x.get.num}, ${y.get.num})_${x.get.prime}"

  def equals(obj: PointFE): Boolean =
    x == obj.x && y == obj.y && a == obj.a && b == obj.b

  def !=(point: PointFE): Boolean =
    x != point.x || y != point.y || a != point.a || b != point.b

  def FE(int: BigInt): FieldElement = FieldElement(Some(int), a.prime)

  def someFE(int: Option[BigInt]): Option[FieldElement] = Some(FE(int.get))

  def requireSameCurve(that: PointFE): Unit =
    if (a != that.a || b != that.b)
      throw new RuntimeException(
        s"Points $this, $that are not on the same curve"
      )

}

case class Point(x: Option[BigInt] = None,
                 y: Option[BigInt] = None,
                 a: BigInt,
                 b: BigInt) {

  if (x.isDefined && y.isDefined)
    if (y.get.pow(2) != x.get.pow(3) + (a * x.get) + b)
      throw new RuntimeException(s"($x, $y) is not on the curve")

  override def toString: String =
    if (x.isEmpty) s"Point(infinity)" else s"Point($x, $y, $a, $b)"
  def equals(that: Point): Boolean =
    x == that.x && y == that.y && a == that.a && b == that.b
  def same(point: Point): Boolean = this == point && y.get == 0
  def isAtInfinity(x: Option[BigInt]): Boolean = x.isEmpty

  def !=(that: Point): Boolean =
    x != that.x || y != that.y || a != that.a || b != that.b

  def *(coeff: Int): Point = {
    var product = Point(a = a, b = b)
    for (_ <- 1 to coeff) product += this
    product
  }

  def requireSameCurve(that: Point): Unit =
    if (a != that.a || b != that.b)
      throw new RuntimeException(
        s"Points $this, $that are not on the same curve"
      )

  def +(that: Point): Point = {
    requireSameCurve(that)
    // at infinity
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

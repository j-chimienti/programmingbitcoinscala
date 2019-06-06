package models

trait PointTFE[T] {

  if (x.isDefined && y.isDefined)
    if (y.get ** 2 != x.get ** 3 + (a * x.get) + b)
      throw new RuntimeException(s"(${x.get}, ${y.get}) is not on the curve")

  def x: Option[FieldElement]
  def y: Option[FieldElement]
  def a: FieldElement
  def b: FieldElement
  def same(point: Point): Boolean = {
    this == point && y.get == 0
  }

  override def toString: String =
    if (x.isEmpty) "Point(infinity)"
    else s"Point(${x.get.num}, ${y.get.num})_${x.get.prime}"

  def equals(obj: PointFE): Boolean =
    x == obj.x && y == obj.y && a == obj.a && b == obj.b

  def !=(point: PointFE): Boolean =
    x != point.x || y != point.y || a != point.a || b != point.b

  def FE(int: BigInt): FieldElement = FieldElement(Some(int), a.prime)

  def someFE(int: Option[BigInt]): Option[FieldElement] = Some(FE(int.get))

}

case class PointFE(x: Option[FieldElement] = None,
                   y: Option[FieldElement] = None,
                   a: FieldElement,
                   b: FieldElement)
    extends PointTFE[PointFE] {

  def requireSameCurve(point: PointFE): Unit =
    if (a != point.a || b != point.b)
      throw new RuntimeException(
        s"Points $this, $point are not on the same curve"
      )

  @throws(classOf[RuntimeException])
  def +(point: PointFE): PointFE = {
    requireSameCurve(point)
    if (this == point && y.get == 0) PointFE(None, None, a, b)
    // Case 0.0: this is the point at infinity, return other point
    else if (x.isEmpty) point
    // Case 0.0: other is the point at infinity, return this point
    else if (point.x.isEmpty) PointFE(x, y, a, b)
    // Case 1: self.x == other.x, self.y != other.y
    else if (x == point.x && y != point.y)
      PointFE(None, None, a, b)
    // Case 2: self.x != other.x
    else if (x != point.x) {
      val denom: FieldElement = (point.x.get - x.get)
      val slope: FieldElement = (point.y.get - y.get) / denom
      val x3 = slope ** 2 - x.get - point.x.get
      val y3 = slope * (x.get - x3) - y.get
      PointFE(Some(x3), Some(y3), a, b)
    }
    // Case 3: self.x == other.x, self.y == other.y
    else {
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

}

package models


object Point {

  def apply(x: FieldElement, y: FieldElement, a: FieldElement, b: FieldElement): PointFE = {

    PointFE(x, y, a, b)

  }
}



case class PointFE(x: FieldElement, y: FieldElement, a: FieldElement, b: FieldElement) {

  val zero: Int = 0

  if (!(x.num.isInfinity && y.num.isInfinity)) {

    if (y ** 2 != x ** 3 + (a * x) + b) {
      throw new RuntimeException(s"($x, $y) is not on the curve")
    }

  }

  def equals(obj: PointFE): Boolean =
    x == obj.x && y == obj.y && a == obj.a && b == obj.b


  def !=(point: PointFE): Boolean = {


    x != point.x || y != point.y || a != point.a || b != point.b

  }

  override def toString: String = {

  if (x.num.isInfinity) {
    "Point(infinity)"
  } else {

    s"Point(${x.num}, ${y.num})_${x.prime}"
  }

  }

  @throws(classOf[RuntimeException])
  def +(point: PointFE): PointFE = {


    if (a != point.a || b != point.b) {

      throw new RuntimeException(s"PointFEs $this, $point are not on the same curve")
    }

    /*

    if (this == point && y == zero) {

      PointFE(a = a, b = b)

      // Case 0.0: this is the point at infinity, return other point
    } else if (x.isInfinity) {
      point

      // Case 0.0: other is the point at infinity, return this point
    } else if (point.x.isInfinity) {

      this
      // Case 1: self.x == other.x, self.y != other.y
    } else
     */

    if (x == point.x && y != point.y) {

      val _x = FieldElement(num = Double.PositiveInfinity.toLong, prime = x.prime)
      val _y = FieldElement(num = Double.PositiveInfinity.toLong, prime = x.prime)
      PointFE(_x, _y, a = a, b = b)

      //      # Case 2: self.x != other.x
    } else if (x != point.x) {


      val slope = (point.y - y) / (point.x - x)
      val x3 = slope ** 2 - x - point.x
      val y3 = slope * (x - x3) - y

      PointFE(x3, y3, a, b)


    }

      // # Case 3: self.x == other.x, self.y == other.y
    else {

      println("case 3 not implmented")
      this
    }


//    else {
//
//      val s = (3 * x ** 2 + a) / (2 * y)
//
//      val xx = s ** 2 - 2 * x
//
//      val yy = s * (x - xx) - y
//
//      PointFE(xx, yy, a, b)
//    }
  }


}

case class Point(x: Double = Double.PositiveInfinity, y: Double = Double.PositiveInfinity, a: Double, b: Double)  {



  val zero: Int = 0

  if (!(x.isInfinity && y.isInfinity)) {

    if (Math.pow(y, 2) != Math.pow(x, 3) + (a * x) + b) {
      throw new RuntimeException(s"($x, $y) is not on the curve")
    }

  }

  def equals(obj: Point): Boolean =
    x == obj.x && y == obj.y && a == obj.a && b == obj.b


  def !=(point: Point): Boolean = {


    x != point.x || y != point.y || a != point.a || b != point.b

  }


  @throws(classOf[RuntimeException])
  def +(point: Point): Point = {


    if (a != point.a || b != point.b) {

      throw new RuntimeException(s"Points $this, $point are not on the same curve")
    }

    if (this == point && y == zero) {

      Point(a = a, b = b)

      // Case 0.0: this is the point at infinity, return other point
    } else if (x.isInfinity) {
      point

      // Case 0.0: other is the point at infinity, return this point
    } else if (point.x.isInfinity) {

      this
      // Case 1: self.x == other.x, self.y != other.y
    } else if (x == point.x && y != point.y) {

      Point(a = a, b = b)

      //      # Case 2: self.x != other.x
    } else if(x != point.x) {


      val slope = (point.y - y) / (point.x - x)
      val x3 = Math.pow(slope, 2) - x - point.x
      val y3 = slope * (x - x3) - y

      Point(x3, y3, a, b)

      // # Case 3: self.x == other.x, self.y == other.y

    } else {

      val s = (3 * Math.pow(x, 2) + a) / (2 * y)

      val xx = Math.pow(s, 2) - 2 * x

      val yy = s * (x - xx) - y

      Point(xx, yy, a, b)
    }
  }



  override def toString: String = {

    if (x.isInfinity) {

      s"Point(infinity)"
    } else {

      s"Point($x, $y, $a, $b)"
    }

  }


}

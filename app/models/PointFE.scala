package models


//object Point {
//
//  def apply(x: FieldElement, y: FieldElement, a: FieldElement, b: FieldElement): PointFE = {
//
//    PointFE(x, y, a, b)
//
//  }
//}



case class PointFE(x: Option[FieldElement] = None, y: Option[FieldElement] = None, a: FieldElement, b: FieldElement) {

  val zero: Int = 0


  x match {
    case None =>
    case Some(fex) =>
      y match {

        case None =>
        case Some(fey) =>
          if (fey ** 2 != fex ** 3 + (a * fex) + b) {
            throw new RuntimeException(s"($fex, $fey) is not on the curve")
          }
      }
  }


  def equals(obj: PointFE): Boolean =
    x == obj.x && y == obj.y && a == obj.a && b == obj.b


  def !=(point: PointFE): Boolean = {


    x != point.x || y != point.y || a != point.a || b != point.b

  }

  override def toString: String = {

    if (x.isEmpty) {
      "Point(infinity)"
    } else {

      s"Point(${x.get.num}, ${y.get.num})_${x.get.prime}"
    }

  }

  // fixme:
  @throws(classOf[RuntimeException])
  def +(point: PointFE): PointFE = {


    if (a != point.a || b != point.b) {

      throw new RuntimeException(s"PointFEs $this, $point are not on the same curve")
    }
    if (this == point && y.get == zero) {

      PointFE(a = a, b = b)

      // Case 0.0: this is the point at infinity, return other point
    } else if (x.isEmpty) {
      point

      // Case 0.0: other is the point at infinity, return this point
    } else if (point.x.isEmpty) {

      this
      // Case 1: self.x == other.x, self.y != other.y
    } else if (x == point.x && y != point.y) {

      PointFE(None, None, a = a, b = b)

      //      # Case 2: self.x != other.x
    } else if (x != point.x) {


        val slope = (point.y.get - y.get) / (point.x.get - x.get)
        val x3 = slope ** 2 - x.get - point.x.get
        val y3 = slope * (x.get - x3) - y.get
        PointFE(Some(x3), Some(y3), a, b)

    }
    // # Case 3: self.x == other.x, self.y == other.y

      else {

        val numerator = x.get ** 2 + a
        val s = (numerator * 3) / (y.get * 2)

        val xx = s ** 2 - x.get * 2

        val yy = s * (x.get - xx) - y.get

        PointFE(Some(xx), Some(yy), a, b)
      }
  }


  def *(coeff: Int): PointFE = {

    var product =  PointFE(None, None, a = a, b = b)

    for (_ <- 1 to coeff) {

      product += this

    }

    product
  }



}

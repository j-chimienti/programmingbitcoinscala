package models



case class Point(x: Option[Double] = None, y: Option[Double] = None, a: Double, b: Double)  {



  val zero: Int = 0


  if (x.isDefined && y.isDefined) {


    if (Math.pow(y.get, 2) != Math.pow(x.get, 3) + (a * x.get) + b) {
      throw new RuntimeException(s"($x, $y) is not on the curve")
    }

  }



  def equals(obj: Point): Boolean =
    x == obj.x && y == obj.y && a == obj.a && b == obj.b


  def !=(point: Point): Boolean = {


    x != point.x || y != point.y || a != point.a || b != point.b

  }

  /*

  def __mul__(self, other):
        if self.prime != other.prime:
            raise RuntimeError('Primes must be the same')
        # self.num and other.num are the actual values
        num = (self.num * other.num) % self.prime
        # self.prime is what you'll need to mod against
        prime = self.prime
        # You need to return an element of the same class
        # use: self.__class__(num, prime)
        return self.__class__(num, prime)
   */


  def *(coeff: Int) = {

    // product = self.__class__(None, None, self.a, self.b) # (1)

    var product = Point(a = a, b = b)
    for (_ <- 1 to coeff) {

      product += this

    }

    product
  }


  @throws(classOf[RuntimeException])
  def +(point: Point): Point = {


    if (a != point.a || b != point.b) {

      throw new RuntimeException(s"Points $this, $point are not on the same curve")
    }

    if (this == point && y.get == zero) {

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
    } else if(x != point.x) {


      val slope = (point.y.get - y.get) / (point.x.get - x.get)
      val x3 = Math.pow(slope, 2) - x.get - point.x.get
      val y3 = slope * (x.get - x3) - y.get

      Point(Some(x3), Some(y3), a, b)

      // # Case 3: self.x == other.x, self.y == other.y

    } else {

      val s = (3 * Math.pow(x.get, 2) + a) / (2 * y.get)

      val xx = Math.pow(s, 2) - 2 * x.get

      val yy = s * (x.get - xx) - y.get

      Point(Some(xx), Some(yy), a, b)
    }
  }



  override def toString: String = {

    if (x.isEmpty) {

      s"Point(infinity)"
    } else {

      s"Point($x, $y, $a, $b)"
    }

  }


}

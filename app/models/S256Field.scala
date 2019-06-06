package models

object S256Field {
  def apply(n: Option[BigInt] = None): S256Field = new S256Field(n)

}

class S256Field(n: Option[BigInt] = None)
    extends FieldElement(n, secp256kk1.P) {
  def hex: String = {
    val fill = 64
    val len = n.toString.length
    val toFill = Math.max(0, fill - len)
    var str = ""
    for (_ <- 1 to toFill) {
      str += "0"
    }
    str += n.toString
    // println(s"length = ${str.length}, 64 = ${str.length == 64}")
    //require(str.length == 64)
    str
  }
  override def toString: String = hex

  def sqrt: S256Field = {
    val (divideToIntegral, remainder) = BigDecimal(prime + 1) /% 4
    this ** divideToIntegral.toBigInt()
  }

  def /(s256Field: S256Field): S256Field = {
    val result = /(s256Field.num, s256Field.prime)
    S256Field(result.num)
  }

  def *(s256Field: S256Field): S256Field = {
    val result = mult(s256Field.num.get)
    S256Field(Some(result))
  }

  override def -(fe: FieldElement): S256Field =
    S256Field(Some(minus(fe.num, fe.prime)))

  def -(s256Field: S256Field): S256Field =
    S256Field(Some(minus(s256Field.num, s256Field.prime)))

  override def +(fe: FieldElement): S256Field =
    S256Field(Some(add(fe.num, fe.prime)))

  override def *(coeff: Int): S256Field =
    S256Field(Some(mult(coeff)))

  override def **(_num: BigInt): S256Field =
    S256Field(Some(pow(_num)))

}

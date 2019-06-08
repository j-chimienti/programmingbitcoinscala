package models

import scodec.bits.ByteVector
import scala.language.postfixOps

object S256Field {

  def apply(n: String): S256Field = S256Field(Some(BigInt(n, 16)))
  def apply(n: Int): S256Field = S256Field(Some(BigInt(n)))
  def apply(n: BigInt): S256Field = S256Field(Some(n))
  def apply(n: Option[BigInt] = None): S256Field =
    new S256Field(n)

}

class S256Field(n: Option[BigInt] = None)
    extends FieldElement(n, secp256kk1.P) {
  def hex: String = n.get.toString(16) zfill

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
  override def *(coeff: Int): S256Field =
    S256Field(Some(mult(coeff)))

  override def -(fe: FieldElement): S256Field =
    S256Field(Some(minus(fe.num, fe.prime)))

  def -(s256Field: S256Field): S256Field =
    S256Field(Some(minus(s256Field.num, s256Field.prime)))

  override def +(fe: FieldElement): S256Field =
    S256Field(Some(add(fe.num, fe.prime)))

  override def **(_num: BigInt): S256Field =
    S256Field(Some(pow(_num)))

}

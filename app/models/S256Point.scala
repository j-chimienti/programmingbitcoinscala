package models

import fr.acinq.bitcoin.{Base58Check, Crypto}
import scodec.bits.ByteVector

import scala.language.postfixOps

/**
  *   The Public Key
  * @param x coordinate
  * @param y coordinate
  * @param a Field of coeff of secpk256k1 curve
  * @param b Field of coeff of secpk256k1 curve
  */
class S256Point(x: Option[S256Field] = None,
                y: Option[S256Field] = None,
                a: S256Field,
                b: S256Field)
    extends PointFE(x, y, a, b) {

  import secp256kk1._
  final val bits = 256

  /**
    * Standards for Efficient Cryptography
    * Serialize the data
    * @param compressed form of sec format wanted
    * @return the 33 byte if compressed and 65 if not compressed
    */
  def sec(compressed: Boolean = true): ByteVector = {

    val X = x.get.num.get
    if (compressed) {
      val byte = if (y.get.num.get.mod(2) == 0) 2.toByte else 3.toByte
      ByteVector(X.toByteArray.+:(byte))
    } else {
      ByteVector((X.toByteArray ++ y.get.num.get.toByteArray).+:(4.toByte))
    }

  }

  def address(compressed: Boolean = true, testnet: Boolean = false): String = {
    val prefix = if (testnet) 0x6f.toByte else 0x00.toByte
    val SEC = sec(compressed)
    val h160 = Crypto.hash160(SEC) // 20 byte
    val raw = h160.+:(prefix)
    val checksum = Base58Check.checksum(raw)
    (raw ++ checksum).toBase58
  }

  def verify(z: String, sig: Signature): Boolean =
    verify(BigInt(z, 16), sig)

  def verify(z: BigInt, sig: Signature): Boolean = {
    // remember 1/s = pow(s, N-2, N)
    val s_inv = sig.s.modPow(N - 2, N)
    // u = z / s
    val u = (z * s_inv).mod(N)
    // v = r / s
    val v = (sig.r * s_inv).mod(N)
    // u*G + v*P should have as the x coordinate, r
    val total: S256Point = G * u + this * v
    total.x.get.num.get == sig.r
  }

  def *(coeff: BigInt): S256Point = {

    var coefficient = coeff.mod(N)
    var current: S256Point = this
    var result: S256Point = S256Point(None, None)
    for (_ <- 1 to bits) {
      if ((coefficient & 1) != 0) {
        result = result + current
      }
      current += current
      coefficient >>= 1
    }
    result
  }
  def +(point: S256Point): S256Point = {
    requireSameCurve(point)
    add(point)
  }

  def add(point: S256Point): S256Point = {
    requireSameCurve(point)
    if (this == point && y.get.num.get == 0)
      S256Point(None, None)
    // Case 0.0: this is the point at infinity, return other point
    else if (x.isEmpty) point
    // Case 0.0: other is the point at infinity, return this point
    else if (point.x.isEmpty) S256Point(x, y)
    // Case 1: self.x == other.x, self.y != other.y
    else if (x == point.x && y != point.y)
      S256Point(None, None)
    // Case 2: self.x != other.x
    else if (x != point.x) {
      val numerator: FieldElement = point.y.get - y.get
      val denom: FieldElement = point.x.get - x.get
      val slope: FieldElement = numerator / denom
      val x3: FieldElement = slope ** 2 - x.get - point.x.get
      val y3: FieldElement = slope * (x.get - x3) - y.get
      S256Point(x3, y3)
    }
    // Case 3: self.x == other.x, self.y == other.y
    else {
      val numerator = x.get ** 2 + a
      val s = (numerator * 3) / (y.get * 2)
      val xx = s ** 2 - x.get * 2
      val yy = s * (x.get - xx) - y.get
      S256Point(Some(xx), Some(yy))
    }
  }
  override def toString: String =
    x match {
      case None     => "S256Point(infinity)"
      case Some(xx) => s"S256Point($xx, ${y.get})"
    }

}

object S256Point {

  def apply(x: String, y: String): S256Point =
    S256Point.apply(BigInt(x, 16), BigInt(y, 16))

  def apply(x: FieldElement, y: FieldElement): S256Point =
    S256Point.apply(x.num.get, y.num.get)

  def apply(x: ByteVector, y: ByteVector): S256Point =
    S256Point.apply(BigInt(x.toArray), BigInt(y.toArray))

  def apply(x: BigInt, y: BigInt): S256Point =
    S256Point.apply(S256Field(x), S256Field(y))

  def apply(x: S256Field, y: S256Field): S256Point =
    S256Point.apply(Some(x), Some(y))

  def apply(x: Option[S256Field] = None,
            y: Option[S256Field] = None): S256Point =
    new S256Point(x, y, S256Field(secp256kk1.A), S256Field(secp256kk1.B))

  def apply: S256Point =
    new S256Point(None, None, S256Field(secp256kk1.A), S256Field(secp256kk1.B))

  /**
    *
    * Returns a Point object from a compressed sec binary (not hex)
    * @param sec_bin sec_binary ByteVector
    * @return
    */
  def parse(sec_bin: ByteVector): S256Point = {

    if (sec_bin.head == 4) {
      val x = sec_bin.slice(1, 33)
      val y = sec_bin.slice(33, 65)
      S256Point(x, y)
    }
    val is_even = sec_bin.head == 2.toByte
    val x = S256Field(Some(BigInt(sec_bin.toArray.drop(1))))
    // right side of the equation y^2 = x^3 + 7
    val alpha: S256Field = x ** 3 + S256Field(secp256kk1.B)
    // solve for left side
    val beta = alpha.sqrt
    val (even_beta, odd_beta) = if (beta.num.get.mod(2) == 0) {
      val even_beta = beta
      val odd_beta = S256Field(secp256kk1.P - beta.num.get)
      (even_beta, odd_beta)
    } else {
      val even_beta = S256Field(secp256kk1.P - beta.num.get)
      val odd_beta = beta
      (even_beta, odd_beta)
    }
    S256Point(x, if (is_even) even_beta else odd_beta)
  }

}

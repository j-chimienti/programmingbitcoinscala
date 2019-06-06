package models

import java.io.ByteArrayInputStream
import java.nio.{ByteBuffer, ByteOrder}

import fr.acinq.bitcoin.Base58.Prefix
import fr.acinq.bitcoin.ByteVector32
import fr.acinq.bitcoin.Protocol._
import scodec.bits.ByteVector

object S256Point {
  val A = S256Field(Some(secp256kk1.A))
  val B = S256Field(Some(secp256kk1.B))
  def apply(x: Option[S256Field] = None,
            y: Option[S256Field] = None): S256Point =
    new S256Point(x, y, A, B)

  def apply(x: String, y: String): S256Point =
    S256Point.apply(BigInt(x, 16), BigInt(y, 16))

  def apply(x: BigInt, y: BigInt): S256Point =
    S256Point.apply(Some(S256Field(Some(x))), Some(S256Field(Some(y))))

  def apply(x: FieldElement, y: FieldElement): S256Point =
    S256Point(Some(S256Field(x.num)), Some(S256Field(y.num)))

  def apply: S256Point = new S256Point(None, None, A, B)

  // eturns a Point object from a compressed sec binary (not hex)
  def parse(sec_bin: Array[Byte]): S256Point = {

    if (sec_bin.head == 4) {
      val x = BigInt(sec_bin.slice(1, 33))
      val y = BigInt(sec_bin.slice(33, 65))
      S256Point(Some(S256Field(Some(x))), Some(S256Field(Some(y))))
    }
    val is_even = sec_bin.head == 2
    val x = S256Field(Some(BigInt(sec_bin.slice(1, -1))))
    // right side of the equation y^2 = x^3 + 7
    val alpha: S256Field = x ** 3 + S256Field(Some(secp256kk1.B))
    // solve for left side
    val beta = alpha.sqrt
    val (even_beta, odd_beta) = if (beta.num.get.mod(2) == 0) {
      val even_beta = beta
      val odd_beta = S256Field(Some(secp256kk1.P - beta.num.get))
      (even_beta, odd_beta)
    } else {
      val even_beta = S256Field(Some(secp256kk1.P - beta.num.get))
      val odd_beta = beta
      (even_beta, odd_beta)
    }
    S256Point(Some(x), if (is_even) Some(even_beta) else Some(odd_beta))
  }

}

class S256Point(x: Option[S256Field] = None,
                y: Option[S256Field] = None,
                a: S256Field,
                b: S256Field)
    extends PointFE(x, y, a, b) {

  import HashHelper._
  import secp256kk1._
  val bits = 256
  override def toString: String =
    x match {
      case None     => "S256Point(infinity)"
      case Some(xx) => s"S256Point($xx, ${y.get})"
    }
  def *(coeff: BigInt): S256Point = {

    var coefficient = coeff.mod(secp256kk1.N)
    var current: S256Point = this
    var result: S256Point = S256Point(None, None)
    for (_ <- 1 to bits) {
      if ((coefficient & 1) != 0) {
        result = result + current
      }
      current = current + current
      coefficient >>= 1
    }
    result
  }
  @throws(classOf[RuntimeException])
  def +(point: S256Point): S256Point = {
    requireSameCurve(point)
    add(point)
  }

  // fixme:
  @throws(classOf[RuntimeException])
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
  def sec(compressed: Boolean = true) = {

    val xBv =
      ByteVector(x.get.num.get.toByteArray)
    if (compressed) {
      val byte = if (y.get.num.get.mod(2) == 0) 2.toByte else 3.toByte
      xBv.take(33).update(0, byte)
    } else {
      val byte = 4.toByte
      val yy = ByteVector(y.get.num.get.toByteArray)
      (xBv ++ yy).take(65).update(0, byte)
    }

  }

  /**
    * Returns the address string
    * @param compressed
    * @param testnet
    */
  def address(compressed: Boolean = true, testnet: Boolean = false): String = {

    // get the sec
    val _sec: ByteVector = sec(compressed)
    // hash160 the sec
    val h160: ByteVector = hash160(_sec)
    val prefix: Byte =
      if (testnet) Prefix.PubkeyAddressTestnet else Prefix.PubkeyAddress
    val raw: ByteVector = ByteVector(Array(prefix) ++ h160.toArray)
    // checksum is first 4 bytes of double_sha256 of raw
    // val who = double_sha256(raw).toArray.take(4)
    val checksum: Array[Byte] = hash256(raw).toArray.take(4)

    // encode_base58 the raw + checksum
    //val address = encode_base58(raw ++ ByteVector(checksum), prefix)
    val address = encode_base58S(raw ++ ByteVector(checksum))
    address
  }

  def verify(z: String, sig: Signature): Boolean =
    verify(BigInt(z, 16), sig)

  def verify(z: BigInt, sig: Signature): Boolean = {
    // remember 1/s = pow(s, N-2, N)
    val s_inv = sig.s.modPow(N - 2, N)
    // u = z / s
    val u = z * s_inv.mod(N)
    // v = r / s
    val v = sig.r * s_inv.mod(N)
    // u*G + v*P should have as the x coordinate, r
    val total = G * u + this * v
    total.x.get.num.get == sig.r
  }

}

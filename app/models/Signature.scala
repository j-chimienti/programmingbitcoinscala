package models

import java.io.ByteArrayInputStream
import java.math.BigInteger

import scodec.bits.ByteVector

case class Signature(r: BigInt, s: BigInt) {

  override def toString: String = s"Signature($r,$s)"

  /**
    *   The serialized Signature.
    *   Signatures cannot be compressed due as s cannot be derived from r
    * @return
    */
  def der: Array[Byte] = {
    var rbin: Array[Byte] = r.toByteArray
    var result = Array.empty[Byte]
    // if rbin has a high bit, add a 00
    if (rbin.head >= 128) rbin = Array(0x00.toByte) ++ rbin
    result = result ++ Array(2.toByte, rbin.length.toByte) ++ rbin
    var sbin = s.toByteArray
    // if sbin has a high bit, add a 00
    if (sbin.head >= 128) sbin = Array(0x00.toByte) ++ sbin
    result = result ++ Array(2.toByte, sbin.length.toByte) ++ sbin
    Array(0x30.toByte, result.length.toByte) ++ result

  }

}

object Signature {

  def apply(r: Int, s: Int): Signature = Signature(BigInt(r), BigInt(s))
  def apply(r: String, s: String): Signature =
    Signature(BigInt(r, 16), BigInt(s, 16))

  def apply(signatureBin: String): Signature = parse(signatureBin.getBytes)
  def parse(signature_bin: Array[Byte]): Signature =
    parse(new ByteArrayInputStream(ByteVector(signature_bin).toArray))

  def parse(input: ByteArrayInputStream): Signature = {
    require(input.read() == 0x30, "Bad Signature")

    def readLength: Int = {
      val len = input.read()
      if ((len & 0x80) == 0) len
      else {
        var n = len - 0x80
        var len1 = 0
        while (n > 0) {
          len1 = (len1 << 8) + input.read()
          n = n - 1
        }
        len1
      }
    }

    readLength
    require(input.read() == 0x02, "Bad Signature")
    val lenR = readLength
    val r = new Array[Byte](lenR)
    input.read(r)
    require(input.read() == 0x02, "Bad Signature")
    val lenS = readLength
    val s = new Array[Byte](lenS)
    input.read(s)
    Signature(new BigInteger(1, r), new BigInteger(1, s))
  }

}

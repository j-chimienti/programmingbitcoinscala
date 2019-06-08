package models

import fr.acinq.bitcoin.ByteVector32
import org.bouncycastle.crypto.digests.RIPEMD160Digest
import scodec.bits.ByteVector

object RIPEMD160 {

  /**
    *
    * @param input
    * @return hex string
    */
  def decrypt(input: String): Array[Byte] = {
    val raw = input.getBytes("US-ASCII")
    decrypt(ByteVector32(ByteVector(raw)))
    // out.map("%02x".format(_)).mkString
  }

  def decrypt(bv: ByteVector32): Array[Byte] = {

    val messageDigest = new RIPEMD160Digest()
    messageDigest.update(bv.toArray, 0, bv.toArray.length)
    val out: Array[Byte] = Array.fill[Byte](messageDigest.getDigestSize)(0)
    messageDigest.doFinal(out, 0)
    out
  }

  def test(): Unit = {
    val result = decrypt("Rosetta Code")
    val r = result.map("%02x".format(_)).mkString
    assert(r == "b3be159860842cebaa7174c8fff0aa9e50a5199f")

  }
}

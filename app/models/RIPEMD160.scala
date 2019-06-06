package models

import org.bouncycastle.crypto.digests.RIPEMD160Digest

object RIPEMD160 {

  /**
    *
    * @param input
    * @return hex string
    */
  def decrypt(input: String): String = {
    val raw = input.getBytes("US-ASCII")
    val messageDigest = new RIPEMD160Digest()
    messageDigest.update(raw, 0, raw.length)
    val out: Array[Byte] = Array.fill[Byte](messageDigest.getDigestSize)(0)
    messageDigest.doFinal(out, 0)
    out.map("%02x".format(_)).mkString
  }

  def test(): Unit = {
    val result = decrypt("Rosetta Code")
    assert(result == "b3be159860842cebaa7174c8fff0aa9e50a5199f")

  }
}

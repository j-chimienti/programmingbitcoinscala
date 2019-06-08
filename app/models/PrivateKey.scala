package models

import scala.language.postfixOps
import scala.util.Random
import fr.acinq.bitcoin.{Base58Check, ByteVector32, Crypto}
import scodec.bits.ByteVector

case class PrivateKey(secret: ByteVector, point: S256Point) {

  def hex: String = secret.toHex zfill

  def sign(z: BigInt): Signature = {

    val k = BigInt(256, Random)
    val r = (secp256kk1.G * k).x.get.num.get
    val kInv = k.modPow(secp256kk1.N - 2, secp256kk1.N)
    var s = ((z + r * BigInt(secret.toArray)) * kInv).mod(secp256kk1.N)
    if (s > secp256kk1.N / 2) s = secp256kk1.N - s
    Signature(r, s)
  }

  /**
    *
    * Wallet Import Format will compress the private key.
    * Not needed often as the key is not broadcast.
    * Useful to transfer b/w wallets
    * @param compressed
    * @param testnet
    * @return
    */
  def wif(compressed: Boolean = true, testnet: Boolean = false): String = {

    // convert secret to 32 bytes

    val prefix = if (testnet) 0xef.toByte else 0x80.toByte
    //var bytes = secret.padLeft(32).toArray.+:(prefix) // (2)
    var bytes = secret.toArray.+:(prefix) // (2)
    if (compressed) bytes = bytes.:+(0x01.toByte)
    val check = HashHelper.hash256(ByteVector(bytes)).take(4)
    val foo = bytes ++ check.toArray
    ByteVector(foo).toBase58
  }
  override def toString: String = s"PrivateKey($hex, $point)"
}

object PrivateKey {

  /**
    *   String must be in hex format or will throw error
    * @param data
    * @return
    */
  def fromHex(data: String): PrivateKey =
    PrivateKey(ByteVector.fromValidHex(data))

  def fromBase58(data: String): PrivateKey =
    PrivateKey(ByteVector.fromValidBase58(data))

  def apply(secret: ByteVector): PrivateKey = secret.length match {
    case 32 => PrivateKey(secret, secp256kk1.G * BigInt(secret.toArray))
    case 33 if secret.last == 1 =>
      PrivateKey(secret.take(32), secp256kk1.G * BigInt(secret.toArray))
  }

  def apply(num: Long): PrivateKey =
    new PrivateKey(ByteVector.fromLong(num), secp256kk1.G * num)

}

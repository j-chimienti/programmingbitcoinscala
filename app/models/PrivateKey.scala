package models

import fr.acinq.bitcoin.ByteVector32

import scala.language.postfixOps
import scala.util.Random
import scodec.bits.ByteVector

case class PrivateKey(secret: ByteVector, point: S256Point) {

  def hex: String = secret.toHex zfill

  def publicKey: S256Point = point

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

    val prefix = if (testnet) 0xef.toByte else 0x80.toByte
    var bytes = secret.toArray.+:(prefix)
    if (compressed) bytes = bytes.:+(0x01.toByte)
    val check = HashHelper.hash256(ByteVector(bytes)).take(4)
    val foo = bytes ++ check.toArray
    ByteVector(foo).toBase58
  }
  override def toString: String = s"PrivateKey($hex, $point)"
}

object PrivateKey {

  def apply(num: BigInt): PrivateKey =
    PrivateKey.apply(ByteVector.fromValidHex(num.toString(16)).padLeft(32))

  /**
    *   String must be in hex format or will throw error
    * @param data
    * @return
    */
  def apply(data: String): PrivateKey =
    PrivateKey.apply(ByteVector.fromValidHex(data).padLeft(32))

  def fromBase58(data: String): PrivateKey =
    PrivateKey.apply(ByteVector.fromValidBase58(data).padLeft(32))

  def apply(secret: ByteVector): PrivateKey = secret.length match {
    case 32 => PrivateKey(secret, secp256kk1.G * BigInt(secret.toArray))
    case 33 if secret.last == 1 =>
      PrivateKey(secret.take(32), secp256kk1.G * BigInt(secret.toArray))
  }

}

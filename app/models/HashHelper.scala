package models

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.ByteBuffer
import java.nio.ByteOrder
import util.control.Breaks._

import fr.acinq.bitcoin.Base58.Prefix
import fr.acinq.bitcoin.{Base58Check, ByteVector32}
import scodec.bits.ByteVector
import fr.acinq.bitcoin.Protocol._
import org.spongycastle.crypto.Digest
import org.spongycastle.crypto.digests.{
  RIPEMD160Digest,
  SHA1Digest,
  SHA256Digest
}

class NotImplementedException extends Exception

object HashHelper {

  val BASE58_ALPHABET =
    "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

//  def sha256(input: String): String =
//    MessageDigest
//      .getInstance("sha-256")
//      .digest(input.getBytes())
//      .map("%02x".format(_))
//      .mkString

  // Returns a string version of the bytes
  def bytes_to_str(b: Array[Byte]): String =
    new String(b, StandardCharsets.US_ASCII)

  // Returns a bytes version of the string
  def str_to_bytes(s: String): Array[Byte] =
    s.getBytes(StandardCharsets.US_ASCII)

  /**
    * Takes an input string as little-endian number
    * @param input
    * @return Long
    */
  // https://stackoverflow.com/questions/51123309/converting-a-big-int-to-little-endian-byte-slice
  def littleEndianToInt(input: String): Long = {
    val ba = ByteVector.fromValidHex(input).toArray
    val stream = new ByteArrayInputStream(ba)
    uint32(stream, ByteOrder.LITTLE_ENDIAN)
  }

  // https://gist.github.com/paulononaka/908246

  // https://stackoverflow.com/questions/3842828/converting-little-endian-to-big-endian
  def intToLittleEndian(numero: Array[Byte], allocate: Int): Array[Byte] = {

    //uint32(new ByteArrayInputStream(numero), ByteOrder.LITTLE_ENDIAN)
    val bb = ByteBuffer.allocate(allocate)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.put(numero)
    bb.array()

  }

  def intToLittleEndian(num: Int, allocate: Int): Array[Byte] = {

    val bb = ByteBuffer.allocate(allocate)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(num)
    bb.array()
  }

  @deprecated
  def hash160(s: String): String = {

    "fixme"
    //RIPEMD160.decrypt(sha256(s))
  }

  def hash(digest: Digest)(input: ByteVector): ByteVector = {
    digest.update(input.toArray, 0, input.length.toInt)
    val out = new Array[Byte](digest.getDigestSize)
    digest.doFinal(out, 0)
    ByteVector.view(out)
  }

  def sha1: ByteVector => ByteVector = hash(new SHA1Digest)

  def sha256: ByteVector => ByteVector32 =
    (x: ByteVector) => ByteVector32(hash(new SHA256Digest)(x))

  def ripemd160: ByteVector => ByteVector = hash(new RIPEMD160Digest)

  /**
    * 160 bits bitcoin hash, used mostly for address encoding
    * hash160(input) = RIPEMD160(SHA256(input))
    *
    * @param input array of byte
    * @return the 160 bits BTC hash of input
    */
  def hash160(input: ByteVector): ByteVector = ripemd160(sha256(input))

  def encode_base58S(s: ByteVector) = {

    var count = 0

    breakable {
      for (c <- s.toArray) {
        if (c == 0.toByte) count += 1
        else break
      }
    }
    val prefix = Array.fill(count)(1.toByte)
    // convert from binary to hex, then hex to integer
    var num = BigInt(s.toArray)
    var result = Array.empty[Byte]
    while (num > 0) {
      val (num1, mod) = num /% 58 // divmod(num, 58)
      num = num1
      val bar: String = BASE58_ALPHABET.toList(mod.toInt).toString
      result = result ++ Array(("0x0" + bar).toByte)
    }
    val b = prefix ++ result
    ByteVector(b).toHex
  }
  def encode_base58(bv: ByteVector, prefix: Byte): String =
    Base58Check.encode(prefix, bv)

  def encode_base58(hex: ByteVector, testnet: Boolean): String =
    encode_base58(
      hex,
      if (testnet) Prefix.PubkeyAddressTestnet else Prefix.PubkeyAddress
    )
  def encode_base58(hex: String, testnet: Boolean): String =
    encode_base58(
      ByteVector.fromValidHex(hex),
      if (testnet) Prefix.PubkeyAddressTestnet else Prefix.PubkeyAddress
    )
  def encode_base58(hex: String, prefix: Byte): String =
    encode_base58(ByteVector.fromValidHex(hex), prefix)

  def h160_to_p2sh_address(hex: String, testnet: Boolean = false): String =
    encode_base58(
      ByteVector.fromValidHex(hex),
      if (testnet) Prefix.ScriptAddressTestnet else Prefix.ScriptAddress
    )
  def h160_to_p2pkh_address(bv: ByteVector, testnet: Boolean): String =
    encode_base58(
      bv,
      if (testnet) Prefix.PubkeyAddressTestnet else Prefix.PubkeyAddress
    )
  def h160_to_p2pkh_address(h160: String, testnet: Boolean = false): String =
    h160_to_p2pkh_address(ByteVector.fromValidHex(h160), testnet)

  def decode_base58(input: String): (Byte, ByteVector) =
    Base58Check.decode(input)

  def encode(prefix: Byte, data: ByteVector) = Base58Check.encode(prefix, data)

  /**
    * 256 bits bitcoin hash
    * hash256(input) = SHA256(SHA256(input))
    *
    * @param input array of byte
    * @return the 256 bits BTC hash of input
    */
  def hash256(input: ByteVector) = ByteVector32(sha256(sha256(input)))

  def double_sha256(bv: ByteVector): ByteVector32 = sha256(sha256(bv))

  /**
    * encodes an integer as a varint
    * @param i
    * @return
    */
  def encode_varint(i: Int): Array[Byte] = {
    // 253
    if (i < BigInt("fd", 16)) BigInt(i).toByteArray
    // 65536
    else if (i < BigInt("10000", 16))
      ("\\xfd" + intToLittleEndian(BigInt(i).toByteArray, 4))
        .getBytes(StandardCharsets.UTF_8)
    // 4294967296
    else if (i < BigInt("100000000", 16))
      ("\\xfe" + intToLittleEndian(BigInt(i).toByteArray, 4))
        .getBytes(StandardCharsets.UTF_8)
    // 10000000000000000
    else if (i < BigInt("10000000000000000", 16))
      ("\\xff" + intToLittleEndian(BigInt(i).toByteArray, 4))
        .getBytes(StandardCharsets.UTF_8)
    else throw new Error(s"integer too large: $i")
  }

}

package models

import java.io.{InputStream}
import java.nio.charset.StandardCharsets
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.security.MessageDigest

import fr.acinq.bitcoin.Base58.Prefix
import fr.acinq.bitcoin.{Base58Check, ByteVector32, Protocol}
import scodec.bits.ByteVector
import org.spongycastle.crypto.Digest
import org.spongycastle.crypto.digests.{
  RIPEMD160Digest,
  SHA1Digest,
  SHA256Digest
}

class NotImplementedException extends Exception

object HashHelper {

  val BASE58_ALPHABET: Array[Byte] =
    "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".getBytes()

  def SHA256(input: Array[Byte]): Array[Byte] =
    MessageDigest
      .getInstance("sha-256")
      .digest(input)
  //.map("%02x".format(_))
  //.mkString

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
  def littleEndianToInt(input: String): Long = {
    val ba = ByteVector.fromValidHex(input).toArray
    littleEndianToInt(ba)

  }

  def littleEndianToInt(data: Array[Byte]): Long =
    ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN).getInt() & 0xFFFFFFFFL

  def uint8(stream: InputStream): Int = stream.read()

  def uint16(data: Array[Byte]): Int = {

    val o = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    o.getShort() & 0xFFFF
  }

  def uint32(input: InputStream,
             order: ByteOrder = ByteOrder.LITTLE_ENDIAN): Long = {
    val bin = new Array[Byte](4)
    input.read(bin)
    uint32(bin, order)
  }

  def uint32(input: Array[Byte], order: ByteOrder): Long = {
    val buffer = ByteBuffer.wrap(input).order(order)
    buffer.getInt() & 0xFFFFFFFFL
  }

  // https://gist.github.com/paulononaka/908246

  // https://stackoverflow.com/questions/3842828/converting-little-endian-to-big-endian
  def intToLittleEndian(numero: Array[Byte], allocate: Int): Array[Byte] = {

    val bb = ByteBuffer.allocate(allocate)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.put(numero)
    bb.array()

  }

  def intToLittleEndian(num: Int, allocate: Int = 4): Array[Byte] =
    intToLittleEndian(BigInt(num).toByteArray, allocate)

//  def HASH160(s: ByteVector): Array[Byte] =
//    RIPEMD160.decrypt(sha256(s))

  def hash(digest: Digest)(input: ByteVector): ByteVector = {
    digest.update(input.toArray, 0, input.length.toInt)
    val out = new Array[Byte](digest.getDigestSize)
    digest.doFinal(out, 0)
    ByteVector.view(out)
  }

  /**
    *
    * @param data string (addr)
    * @return the version Byte and the data in base58
    */
  def base58(data: String): (Byte, ByteVector) = Base58Check.decode(data)

  def base58Encode(version: Int, h160: ByteVector): String =
    Base58Check.encode(version, h160)

  def sha1: ByteVector => ByteVector = hash(new SHA1Digest)

  def sha256: ByteVector => ByteVector32 =
    (x: ByteVector) => ByteVector32(hash(new SHA256Digest)(x))

  def ripemd160: ByteVector => ByteVector = hash(new RIPEMD160Digest)

//  def encodeBase58S(s: ByteVector): ByteVector = {
//
//    var count = 0
//    breakable {
//      for (c <- s.toArray) {
//        if (c == 0.toByte) count += 1
//        else break
//      }
//    }
//    val prefix: Array[Byte] = ("1" * count).split("").map(_.toByte)
//    // convert from binary to hex, then hex to integer
//    var num = BigInt(s.toHex, 16)
//    var result = Array.empty[Byte]
//    while (num > 0) {
//      val (num1, mod) = num /% 58
//      num = num1
//      result = result.+:(BASE58_ALPHABET(mod.toInt))
//    }
//    ByteVector(prefix ++ result)
//  }

  def h1602p2sh(hex: String, testnet: Boolean): String =
    Base58Check.encode(
      if (testnet) Prefix.ScriptAddressTestnet else Prefix.ScriptAddress,
      ByteVector.fromValidHex(hex)
    )

  def h1602p2sh(data: ByteVector, testnet: Boolean): String =
    Base58Check.encode(
      if (testnet) Prefix.ScriptAddressTestnet else Prefix.ScriptAddress,
      data
    )

  def h1602p2pkh(h160: String, testnet: Boolean): String =
    Base58Check.encode(
      if (testnet) Prefix.PubkeyAddressTestnet else Prefix.PubkeyAddress,
      ByteVector.fromValidHex(h160)
    )

  def h1602p2pkh(h160: ByteVector, testnet: Boolean): String =
    Base58Check.encode(
      if (testnet) Prefix.PubkeyAddressTestnet else Prefix.PubkeyAddress,
      h160
    )

  def encodeBase58(prefix: Byte, data: ByteVector): String =
    Base58Check.encode(prefix, data)

  def decodeBase58(input: String): (Byte, ByteVector) =
    Base58Check.decode(input)

  /**
    * 256 bits bitcoin hash
    * hash256(input) = SHA256(SHA256(input))
    *
    * @param input array of byte
    * @return the 256 bits BTC hash of input
    */
  def hash256(input: ByteVector) = ByteVector32(sha256(sha256(input)))

  /**
    * Read a variable integer from a stream
    * @param stream
    */
  def readVarint(stream: InputStream): Long = stream.read() match {

    case 0xfd                  => Protocol.uint16(stream)
    case 0xfe                  => Protocol.uint32(stream)
    case 0xff                  => Protocol.uint64(stream)
    case value if value < 0xfd => value

  }

  /**
    * encodes an integer as a varint
    * @param i
    * @return
    */
  def encodeVarint(i: Int): Array[Byte] = {
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

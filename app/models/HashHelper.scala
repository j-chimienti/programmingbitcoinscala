package models

import java.io._
import scodec.bits._
import java.nio.charset.StandardCharsets
import java.nio.{ByteBuffer, ByteOrder}
import scala.language.implicitConversions
import org.spongycastle.crypto.Digest
import org.spongycastle.crypto.digests.{
  RIPEMD160Digest,
  SHA1Digest,
  SHA256Digest
}
import fr.acinq.bitcoin.Base58Check

class NotImplementedException extends Exception

object HashHelper {

  import Base58._
  val BASE58_ALPHABET: Array[Byte] =
    "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".getBytes()

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
  def littleEndianToInt(input: String, allocate: Int = 4): Long = {
    val ba = ByteVector.fromValidHex(input).toArray
    littleEndianToInt(ba, allocate)
  }

  def littleEndianToInt(data: Array[Byte], allocate: Int): Long =
    allocate match {

      case 4 => uint32(data, ByteOrder.LITTLE_ENDIAN)
      case 8 => uint64(data, ByteOrder.LITTLE_ENDIAN)
    }

  // https://stackoverflow.com/questions/3842828/converting-little-endian-to-big-endian
  def intToLittleEndian(numero: Array[Byte], allocate: Int): Array[Byte] = {

    val bb = ByteBuffer.allocate(allocate)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.put(numero)
    bb.array()

  }

  def intToLittleEndian(num: Long, allocate: Int): Array[Byte] =
    intToLittleEndian(BigInt(num).toByteArray, allocate)

  def intToLittleEndian(num: Int, allocate: Int = 4): Array[Byte] =
    intToLittleEndian(BigInt(num).toByteArray, allocate)

  def script(input: InputStream): ByteVector = {
    val length = readVarint(input) // read size
    bytes(input, length.toInt) // read bytes
  }
  def bytes(input: InputStream, size: Long): ByteVector =
    bytes(input, size.toInt)

  def bytes(input: InputStream, size: Int): ByteVector = {
    val blob = new Array[Byte](size)
    if (size > 0) {
      val count = input.read(blob)
      if (count < size) throw new IOException("not enough data to read from")
    }
    ByteVector.view(blob)
  }

  /**
    *
    * @param data string (addr)
    * @return the version Byte and the data in base58
    */
  def base58Decode(data: String): (Byte, ByteVector) = Base58Check.decode(data)

  def base58Encode(prefix: Byte, h160: ByteVector): String =
    Base58Check.encode(prefix, h160)

  def ripemd160: ByteVector => ByteVector = hash(new RIPEMD160Digest)

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

  def hash(input: InputStream): ByteVector32 =
    ByteVector32(bytes(input, 32)) // a hash is always 256 bits

  def hash(digest: Digest)(input: ByteVector): ByteVector = {
    digest.update(input.toArray, 0, input.length.toInt)
    val out = new Array[Byte](digest.getDigestSize)
    digest.doFinal(out, 0)
    ByteVector.view(out)
  }

  def sha1: ByteVector => ByteVector = hash(new SHA1Digest)

  def sha256: ByteVector => ByteVector32 =
    (x: ByteVector) => ByteVector32(hash(new SHA256Digest)(x))

  /**
    * 256 bits bitcoin hash
    * hash256(input) = SHA256(SHA256(input))
    *
    * @param input array of byte
    * @return the 256 bits BTC hash of input
    */
  def hash256(input: ByteVector): ByteVector32 =
    ByteVector32(sha256(sha256(input)))

  def readVarint(blob: Array[Byte]): Long =
    readVarint(new ByteArrayInputStream(blob))

  /**
    * Read a variable integer from a stream
    * @param stream
    */
  def readVarint(stream: InputStream): Long = stream.read() match {

    case 0xfd                  => uint16(stream)
    case 0xfe                  => uint32(stream)
    case 0xff                  => uint64(stream)
    case value if value < 0xfd => value

  }

  /**
    * encodes an integer as a varint
    * @param i
    * @return
    */
  def encodeVarint(i: Long): Array[Byte] = {

    val out = new ByteArrayOutputStream()
    if (i < 0xfd) writeUInt8(i.toInt, out)
    else if (i < 0x10000) {
      writeUInt8(0xfd, out)
      writeUInt16(i.toInt, out)

    } else if (i < 0x100000000L) {
      writeUInt8(0xfe, out)
      writeUInt32(i.toInt, out)
    } else {
      writeUInt8(0xff, out)
      writeUInt64(i, out)
    }
    out.toByteArray
  }

  def writeVarint(input: Long, out: OutputStream): Unit = {
    if (input < 0xfdL) writeUInt8(input.toInt, out)
    else if (input < 65535L) {
      writeUInt8(0xfd, out)
      writeUInt16(input.toInt, out)
    } else if (input < 1048576L) {
      writeUInt8(0xfe, out)
      writeUInt32(input.toInt, out)
    } else {
      writeUInt8(0xff, out)
      writeUInt64(input, out)
    }
  }

  def writeScript(input: Array[Byte], out: OutputStream): Unit = {

    writeVarint(input.length.toLong, out)
    out.write(input)
  }

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

  def writeUInt8(input: Int, out: OutputStream): Unit = out.write(input & 0xff)

  def uint16(input: InputStream,
             order: ByteOrder = ByteOrder.LITTLE_ENDIAN): Int = {
    val bin = new Array[Byte](2)
    input.read(bin)
    uint16(bin, order)
  }

  def uint16(input: Array[Byte], order: ByteOrder): Int = {
    val buffer = ByteBuffer.wrap(input).order(order)
    buffer.getShort & 0xFFFF
  }

  def writeUInt16(input: Int,
                  out: OutputStream,
                  order: ByteOrder = ByteOrder.LITTLE_ENDIAN): Unit =
    out.write(writeUInt16(input, order).toArray)

  def writeUInt16(input: Int, order: ByteOrder): ByteVector = {
    val bin = new Array[Byte](2)
    val buffer = ByteBuffer.wrap(bin).order(order)
    buffer.putShort(input.toShort)
    ByteVector.view(bin)
  }

  def writeUInt32(input: Long,
                  out: OutputStream,
                  order: ByteOrder = ByteOrder.LITTLE_ENDIAN): Unit =
    out.write(writeUInt32(input, order).toArray)

  def writeUInt32(input: Long, order: ByteOrder): ByteVector = {
    val bin = new Array[Byte](4)
    val buffer = ByteBuffer.wrap(bin).order(order)
    buffer.putInt((input & 0xffffffff).toInt)
    ByteVector.view(bin)
  }

  def writeUInt32(input: Long): ByteVector =
    writeUInt32(input, ByteOrder.LITTLE_ENDIAN)

  def uint64(input: InputStream,
             order: ByteOrder = ByteOrder.LITTLE_ENDIAN): Long = {
    val bin = new Array[Byte](8)
    input.read(bin)
    uint64(bin, order)
  }

  def uint64(input: Array[Byte], order: ByteOrder): Long = {
    val buffer = ByteBuffer.wrap(input).order(order)
    buffer.getLong()
  }

  def writeUInt64(input: Long,
                  out: OutputStream,
                  order: ByteOrder = ByteOrder.LITTLE_ENDIAN): Unit =
    out.write(writeUInt64(input, order).toArray)

  def writeUInt64(input: Long, order: ByteOrder): ByteVector = {
    val bin = new Array[Byte](8)
    val buffer = ByteBuffer.wrap(bin).order(order)
    buffer.putLong(input)
    ByteVector.view(bin)
  }

  /**
    *  Takes the h160 and returns the scripPubKey
    * @param h160
    * @return scriptPubKey
    */
  def p2pkh_script(h160: ByteVector): ByteVector =
    ByteVector(
      Array(0x76.toByte, 0xa9.toByte, 0x14.toByte) ++ h160.toArray ++ Array(
        0x88.toByte,
        0xac.toByte
      )
    )

  /**
    * see https://en.bitcoin.it/wiki/Protocol_specification
    */
  case class ByteVector32(bytes: ByteVector) {
    require(bytes.size == 32, s"size must be 32 bytes, is ${bytes.size} bytes")

    def reverse: ByteVector32 = ByteVector32(bytes.reverse)

    override def toString: String = bytes.toHex
  }

  object ByteVector32 {
    val Zeroes = ByteVector32(
      hex"0000000000000000000000000000000000000000000000000000000000000000"
    )
    val One = ByteVector32(
      hex"0100000000000000000000000000000000000000000000000000000000000000"
    )

    def fromValidHex(str: String) = ByteVector32(ByteVector.fromValidHex(str))

    implicit def byteVector32toByteVector(h: ByteVector32): ByteVector = h.bytes
  }

}

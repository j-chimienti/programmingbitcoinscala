package models

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  IOException,
  InputStream,
  OutputStream
}
import java.nio.charset.StandardCharsets
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.security.MessageDigest

import fr.acinq.bitcoin.Base58.Prefix
import fr.acinq.bitcoin.Protocol.{
  bytes,
  varint,
  writeUInt16,
  writeUInt32,
  writeUInt64,
  writeUInt8
}
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
  @deprecated(message = "Use uint")
  def littleEndianToInt(input: String): Long = {
    val ba = ByteVector.fromValidHex(input).toArray
    littleEndianToInt(ba)

  }

  @deprecated(message = "Use uint")
  def littleEndianToInt(data: Array[Byte]): Long =
    ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN).getInt() & 0xFFFFFFFFL

  // https://gist.github.com/paulononaka/908246

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

//  def HASH160(s: ByteVector): Array[Byte] =
//    RIPEMD160.decrypt(sha256(s))

  def hash(digest: Digest)(input: ByteVector): ByteVector = {
    digest.update(input.toArray, 0, input.length.toInt)
    val out = new Array[Byte](digest.getDigestSize)
    digest.doFinal(out, 0)
    ByteVector.view(out)
  }

  def hash(input: InputStream): ByteVector32 =
    ByteVector32(bytes(input, 32)) // a hash is always 256 bits

  def script(input: InputStream): ByteVector = {
    val length = varint(input) // read size
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
  def base58(data: String): (Byte, ByteVector) = Base58Check.decode(data)

  def base58Encode(version: Int, h160: ByteVector): String =
    Base58Check.encode(version, h160)

  def sha1: ByteVector => ByteVector = hash(new SHA1Digest)

  def sha256: ByteVector => ByteVector32 =
    (x: ByteVector) => ByteVector32(hash(new SHA256Digest)(x))

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
  def encodeVarint(i: Long) = {

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

  def writeScript(input: Array[Byte], out: OutputStream) = {

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

}

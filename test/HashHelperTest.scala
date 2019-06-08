import java.nio.charset.StandardCharsets

import fr.acinq.bitcoin.{Base58Check, Crypto}
import models.ByteSwapper
import org.scalatest.FunSuite
import scodec.bits.ByteVector

class HashHelperTest extends FunSuite {

  import models.HashHelper._

  test("base58 encode and decode address") {

    val to = "mnrVtF8DWjMu839VW3rBfgYaAfKk8983Xf"
    val (prefix, decoded) = Base58Check.decode(to)
    assert(decoded.toHex == "507b27411ccf7f16f10297de6cef3f291623eddf")
    val addr = Base58Check.encode(prefix, decoded)
    assert(addr == to)
  }

  def assertEqual[T](a: T, b: T): Unit = assert(a == b)

  test("bytes") {

    val b = "hello world".getBytes(StandardCharsets.US_ASCII)
    val s = "hello world"
    // https://stackoverflow.com/questions/5393243/how-do-i-compare-two-arrays-in-scala
    assertEqual(b.deep, str_to_bytes(s).deep)
    assert(BigInt(b) == BigInt(str_to_bytes(s)))
    assert(s == bytes_to_str(b))
  }

  test("littleEndianToInt") {

    val h = "99c3980000000000"
    val want = BigInt(10011545)
    val result = littleEndianToInt(h)
    assert(result == want)
    val h1 = "a135ef0100000000"
    val want1 = BigInt(32454049)
    assert(littleEndianToInt(h1) == want1)
  }

  test("intToLittleEndian") {

    val n = 1
    val want: Array[Byte] =
      Array(0x01.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)
    val res = intToLittleEndian(BigInt(n).toByteArray, 4)
    assert(BigInt(res) == BigInt(want))
    val n1 = 10011545
    val want1 =
      Array(
        0x99.toByte,
        0xc3.toByte,
        0x98.toByte,
        0x00.toByte,
        0x00.toByte,
        0x00.toByte,
        0x00.toByte,
        0x00.toByte
      )
    val result = intToLittleEndian(n1, 8)
    assert(BigInt(want1) == BigInt(result))
  }
  test("base58") {
    val addr = "mnrVtF8DWjMu839VW3rBfgYaAfKk8983Xf"
    val (version, h160) = Base58Check.decode(addr)
    val want = "507b27411ccf7f16f10297de6cef3f291623eddf"
    assert(h160.toHex == want)
    val prefix = 111.toByte
    val got = Base58Check.encode(version, h160)
    assert(got == addr)
  }

  test("p2pkh_address") {
    val h160 =
      ByteVector.fromValidHex("74d691da1574e6b3c192ecfb52cc8984ee7b6c56")
    val want = "1BenRpVUFK65JFWcQSuHnJKzc4M8ZP8Eqa"
    assert(h1602p2pkh(h160, testnet = false) == want)
    val want1 = "mrAjisaT4LXL5MzE81sfcDYKU3wqWSvf9q"
    assert(h1602p2pkh(h160, testnet = true) == want1)
  }

  test("p2sh_address") {
    val h160 =
      "74d691da1574e6b3c192ecfb52cc8984ee7b6c56"
    val want = "3CLoMMyuoDQTPRD3XYZtCvgvkadrAdvdXh"
    assert(h1602p2sh(h160, testnet = false) == want)
    val want1 = "2N3u1R6uwQfuobCqbCgBkpsgBxvr1tZpe7B"
    assert(h1602p2sh(h160, testnet = true) == want1)
  }
}

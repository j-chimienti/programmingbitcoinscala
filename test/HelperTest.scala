import java.nio.charset.StandardCharsets

import models.ByteSwapper
import org.scalatest.FunSuite

class HashHelperTest extends FunSuite {

  import models.HashHelper._

  def assertEqual(a: AnyRef, b: AnyRef): Unit = assert(a == b)


  test("ByteSwapper") {

    val bs = ByteSwapper
    val h = "99c3980000000000".getBytes(StandardCharsets.UTF_16LE)
    val want = BigInt(10011545)


    val ints = h.map(_.toInt)
    val r = bs.swap(ints)


  }

    test("test_bytes") {

      val b = "hello world".getBytes(StandardCharsets.US_ASCII)
      val s = "hello world"
      // https://stackoverflow.com/questions/5393243/how-do-i-compare-two-arrays-in-scala
      assertEqual(b.deep, str_to_bytes(s).deep)
      assertEqual(s, bytes_to_str(b))
    }

    test("test_little_endian_to_int") {

      val h = "99c3980000000000".getBytes(StandardCharsets.US_ASCII)
      val want = BigInt(10011545)
      val result = little_endian_to_int(h)
      assert(result == want.toInt)
      val h1 = "a135ef0100000000".getBytes(StandardCharsets.US_ASCII)
      val want1 = BigInt(32454049)
      assert(little_endian_to_int(h1) == want1.toInt)
    }



    test("test_intToLittleEndian") {

      val n = 1
      val want = Array(1, 0, 0, 0)//  s"\\x01\\x00\\x00\\x00"
      assertEqual(intToLittleEndian(BigInt(n).toByteArray, 4).deep, want.deep)
      val n1 = 10011545
      val want1 = "\\x99\\xc3\\x98\\x00\\x00\\x00\\x00\\x00"
      // assertEqual(intToLittleEndian(BigInt(n1).toByteArray, 8), want1)
    }


//  test("test_base58") {
//    var addr = "mnrVtF8DWjMu839VW3rBfgYaAfKk8983Xf"
//    var h160 = decode_base58(addr).hex()
//    var want = "507b27411ccf7f16f10297de6cef3f291623eddf"
//    assertEqual(h160, want)
//    var got = encode_base58_checksum(Array("\\x6f", h160.getBytes()))
//    assertEqual(got, addr)
//  }(pending)
//
//  test("test_p2pkh_address") {
//    var h160 = "74d691da1574e6b3c192ecfb52cc8984ee7b6c56".getBytes(StandardCharsets.UTF_8)
//    var want = "1BenRpVUFK65JFWcQSuHnJKzc4M8ZP8Eqa"
//    assertEqual(h160_to_p2pkh_address(h160, false), want)
//    var want1 = "mrAjisaT4LXL5MzE81sfcDYKU3wqWSvf9q"
//    assertEqual(h160_to_p2pkh_address(h160, true), want1)
//  }(pending)
//
//  test("test_p2sh_address") {
//    var h160 = "74d691da1574e6b3c192ecfb52cc8984ee7b6c56".getBytes(StandardCharsets.UTF_8)
//    var want = "3CLoMMyuoDQTPRD3XYZtCvgvkadrAdvdXh"
//    assertEqual(h160_to_p2sh_address(h160, false), want)
//    var want1 = "2N3u1R6uwQfuobCqbCgBkpsgBxvr1tZpe7B"
//    assertEqual(h160_to_p2sh_address(h160, true), want1)
//  }(pending)
}

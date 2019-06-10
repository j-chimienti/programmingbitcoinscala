package models

import java.io.{ByteArrayInputStream, InputStream}

import scodec.bits.ByteVector

case class Script(elements: Array[Int]) {

  val p2pkSig = "p2pk sig"
  val p2shSig = "p2sh sig"

  def signature(index: Int = 0): Int =
    `type` match {
      case p2ppk if p2ppk == p2pkSig => elements.head
      case p2sh if p2sh == p2shSig   => elements(index + 1)
      case _ =>
        throw new RuntimeException("Script type needs to be p2pk or p2sh")
    }

  def `type`: String = {

    if (elements.length == 0) "blank"
    else if (elements.head == 0x76 && elements(1) == 0xa9 //&& elements(2).isInstanceOf[Byte]
             && elements(2) == 0x14 &&
             elements(3) == 0x88 && elements(4) == 0xac
             // OP_DUP OP_HASH160 <20 byte hash> <OP_EQUALVERIFY>  <OP_CHECKSIG>
             ) "p2pkh"
    else if (elements.head == 0xa9 && // elements(1).isInstanceOf[Byte]
             elements(1) == 0x14 &&
             elements.last == 0x87
             // OP_HASH160 <20 byte hash> <OP_EQUAL>
             ) "p2sh"
    else
      elements.head match {
        case i
            if Set(0x47, 0x48, 0x49).contains(i) && Set(0x21, 0x41).contains(
              i
            ) =>
          "p2pkh sig"
        case other =>
          if (elements.length > 1 &&
              Set(0x47, 0x48, 0x49)
                .contains(elements(1)) && elements.last
                .asInstanceOf[Byte] == 0xae)
            "p2sh sig"
          else "unknown"
      }
  }

  def serialize: Array[Int] = {

    elements
  }

//  def secPubkey(idx: Int = 0) = {
//
//    `type` match {
//
//      case p2ppk if p2ppk == p2pkSig => elements(1)
//      case p2sh if p2sh == p2shSig   =>
//        // Hack: assumes p2sh is multisig
//        val redeemScript = Script.parse(elements.dropRight(1))
//        redeemScript.elements(idx + 1)
//      case _ =>
//        throw new RuntimeException("Script type needs to be p2pk or p2sh")
//    }
//  }

}

object Script {

  def parse(blob: Array[Byte]): Script = parse(ByteVector.view(blob))

  def parse(data: ByteVector): Script = {
    if (data.length > 10000)
      throw new RuntimeException(s"Script too large, ${data.length}")
    else parse(new ByteArrayInputStream(data.toArray))
  }

  def parse(data: InputStream): Script = {

    var current = data.read()
    var elems = Array.empty[Int]
    var b = Array.empty[Int]
    while (current > 0) {
      val opCode = current
      if (0 < opCode && opCode <= 75) {
        elems = elems.+:(data.read())
      } else {
        elems = elems.+:(opCode)
        current = data.read()
      }
    }
    Script(elems)
  }

}

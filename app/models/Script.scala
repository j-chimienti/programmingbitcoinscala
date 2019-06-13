package models

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  InputStream,
  OutputStream
}

import fr.acinq.bitcoin.Protocol.{
  bytes,
  uint16,
  uint32,
  uint8,
  writeUInt16,
  writeUInt32,
  writeUInt8
}
import scodec.bits.ByteVector

import scala.annotation.tailrec
import ScriptElt._

case class Script(elements: Seq[ScriptElt] = Seq.empty) {

  def signature(index: Int = 0): ScriptElt =
    `type` match {
      case p2ppk if p2ppk == "p2pkh sig" => elements.head
      case p2sh if p2sh == "p2sh sig"    => elements(index + 1)
      case _ =>
        throw new RuntimeException("Script type needs to be p2pk or p2sh")
    }

//  def isPayToScript(script: Seq[ScriptElt]): Boolean = script match {
//    case OP_HASH160 :: OP_PUSHDATA(multisigAddress, _) :: OP_EQUAL :: Nil
//        if multisigAddress.length == 20 =>
//      true
//    case _ => false
//  }

//  def isPayToScript(script: ByteVector): Boolean =
//    script.length == 23 && script(0) == elt2code(OP_HASH160).toByte && script(1) == 0x14 && script(
//      22
//    ) == elt2code(OP_EQUAL).toByte

  def `type`: String = {

//    if (isPayToScript(elements)) {
//      println("PAY TO SCRIPT!!")
//
//    }
    if (elements.isEmpty) "blank"
    // OP_DUP OP_HASH160 <20 byte hash> <OP_EQUALVERIFY>  <OP_CHECKSIG>
    else if (elements.head == OP_DUP && elements(1) == OP_HASH160
             && elements(2)
               .isInstanceOf[OP_PUSHDATA] && elements(3) == OP_EQUALVERIFY && elements(
               4
             ) == OP_CHECKSIG)
      "p2pkh"
    // OP_HASH160 <20 byte hash> <OP_EQUAL>
    else if (elements.head == OP_HASH160 && elements.last == OP_EQUAL) "p2sh"
    else if (elements.head.isInstanceOf[OP_PUSHDATA] && elements(1)
               .isInstanceOf[OP_PUSHDATA])
      if (Set(71, 72, 73).contains(
            elements.head.asInstanceOf[OP_PUSHDATA].data.length.toInt
          ) &&
          Set(33, 65).contains(
            elements(1).asInstanceOf[OP_PUSHDATA].data.length.toInt
          )) "p2pkh sig"
      else "unknown"
    else {

      val foo = Set(71, 72, 73).contains(
        elements(1).asInstanceOf[OP_PUSHDATA].data.length.toInt
      )

      // hack
      val ismulti = elements.last
        .asInstanceOf[OP_PUSHDATA]
        .data
        .toArray
        .last == 0xae.toByte

      // check for

      if (foo && ismulti) "p2sh sig"
      else "unknown"
    }

  }

  def serialize: ByteVector = write(elements)

  def write(script: Seq[ScriptElt]): ByteVector = {
    val out = new ByteArrayOutputStream()
    write(script, out)
    ByteVector.view(out.toByteArray)
  }

  def write(script: Seq[ScriptElt], out: OutputStream): Unit = script match {
    case Nil => ()
    case OP_PUSHDATA(data, length) :: tail
        if data.length < 0x4c && data.length == length =>
      out.write(data.length.toInt); out.write(data.toArray); write(tail, out)
    case OP_PUSHDATA(data, 0x4c) :: tail if data.length < 0xff =>
      writeUInt8(0x4c, out); writeUInt8(data.length.toInt, out);
      out.write(data.toArray); write(tail, out)
    case OP_PUSHDATA(data, 0x4d) :: tail if data.length < 0xffff =>
      writeUInt8(0x4d, out); writeUInt16(data.length.toInt, out);
      out.write(data.toArray); write(tail, out)
    case OP_PUSHDATA(data, 0x4e) :: tail if data.length < 0xffffffff =>
      writeUInt8(0x4e, out); writeUInt32(data.length, out);
      out.write(data.toArray); write(tail, out)
    case op @ OP_PUSHDATA(data, code) :: tail =>
      throw new RuntimeException(s"invalid element $op")
    case head :: tail => out.write(ScriptElt.elt2code(head)); write(tail, out)
  }

  /**
    *   Index isn't used for p2pkh, for p2sh, means one of n pubkeys
    * @param idx
    * @return
    */
  def secPubkey(idx: Int = 0) = {

    `type` match {

      case p2ppk if p2ppk == "p2pkh sig" => elements(1)
      case p2sh if p2sh == "p2sh sig"    =>
        // Hack: assumes p2sh is multisig
        elements.last match {
          case op: OP_PUSHDATA =>
            val redeemScript = Script(op.data)
            redeemScript.elements(idx + 1)
        }
      case _ =>
        throw new RuntimeException("Script type needs to be p2pk or p2sh")
    }
  }

}

object Script {

  /**
    * parse a script from a input stream of binary data
    *
    * @param input input stream
    * @param stack initial command stack
    * @return an updated command stack
    */
  @tailrec
  def parse(input: InputStream,
            stack: collection.immutable.Vector[ScriptElt] =
              Vector.empty[ScriptElt]): List[ScriptElt] = {
    val code = input.read()
    code match {
      case -1 => stack.toList
      case 0  => parse(input, stack :+ OP_0)
      case opCode if opCode > 0 && opCode < 0x4c =>
        parse(input, stack :+ OP_PUSHDATA(bytes(input, opCode), opCode))
      case 0x4c =>
        parse(input, stack :+ OP_PUSHDATA(bytes(input, uint8(input)), 0x4c))
      case 0x4d =>
        parse(input, stack :+ OP_PUSHDATA(bytes(input, uint16(input)), 0x4d))
      case 0x4e =>
        parse(input, stack :+ OP_PUSHDATA(bytes(input, uint32(input)), 0x4e))
      case opCode if code2elt.contains(opCode) =>
        parse(input, stack :+ ScriptElt.code2elt(opCode))
      case opCode =>
        parse(input, stack :+ OP_INVALID(opCode)) // unknown/invalid ops can be parsed but not executed
    }
  }

  def apply(blob: ByteVector): Script = Script(parse(blob))
  def parse(blob: ByteVector): List[ScriptElt] =
    if (blob.length > 10000) throw new RuntimeException("script is too large")
    else parse(new ByteArrayInputStream(blob.toArray))

  def parse(blob: Array[Byte]): List[ScriptElt] = parse(ByteVector.view(blob))

  def apply(blob: Array[Byte]): Script = Script(parse(blob))
  def apply(input: InputStream): Script = Script(parse(input))
//  def parse(data: InputStream): Script = {
//
//    var current = data.read()
//    var elems = Array.empty[Int]
//    var d = Array.empty[Int]
//    while (current != -1) {
//      val opCode = current
//      current = data.read()
//      if (0 < opCode && opCode <= 75) {
//        //elems = elems.+:(current)
//
//        d = d.+:(current)
//      } else {
//        elems = elems.+:(opCode)
//      }
//    }
//    val scriptElems = elems.map(elem => {
//
//      if (ScriptElt.code2elt.contains(elem)) {
//        ScriptElt.code2elt(elem)
//      } else {
//        println(s"unknown $elem")
//        val foo = OP_PUSHDATA(d)
//        ScriptElt.code2elt(0)
//      }
//    })
//    Script(scriptElems)
//  }

}

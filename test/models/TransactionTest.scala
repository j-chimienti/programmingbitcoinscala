package models

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import org.scalatest.FlatSpec
import scodec.bits.ByteVector

class TransactionTest extends FlatSpec {

  behavior of "Transaction"

  it should "parse version and locktime" in {
    val x_hex =
      "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600"
    val tx = Transaction.parse(x_hex)
    assert(tx.version == 1)
    assert(tx.locktime == 410393)
  }
  val txHex =
    "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600"

  it should "parse inputs" in {
    val stream =
      new ByteArrayInputStream(ByteVector.fromValidHex(txHex).toArray)
    val tx = Transaction.parse(stream)
    assert(tx.inputs.length == 1)
    val want =
      "d1c789a9c60383bf715f3f6ad9d14b91fe55f3deb369fe5d9280cb1a01793f81"
    assert(tx.inputs.head.txId.toHex == want)
    assert(tx.inputs.head.prevIdx == 0)
    assert(tx.inputs.head.sequence == 4294967294L)
    val scriptSig =
      "483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a"

    assert(tx.inputs.head.scriptSig.toHex == scriptSig)
  }

  it should "parse outputs" in {

    val tx = Transaction.parse(txHex)
    assert(tx.outputs.length == 2)
    val want = 32454049
    assert(tx.outputs.head.amount == want)
    val want1 =
      "76a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac"
    val out = new ByteArrayOutputStream()

    HashHelper.writeScript(tx.outputs.head.scriptPubKey.toArray, out)
    val result = ByteVector(out.toByteArray)
    assert(result.toHex.drop(2) == want1)
    val want2 = 10011545
    assert(tx.outputs(1).amount == want2)
    val want3 =
      "76a9141c4bc762dd5423e332166702cb75f40df79fea1288ac"

    assert(tx.outputs(1).scriptPubKey.toHex == want3)

  }

//  it should "der signature" in {
//
//    val txStream = new ByteArrayInputStream(
//      ByteVector
//        .fromValidHex(
//          "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600"
//        )
//        .toArray
//    )
//
//    val tx = Transaction.parse(txStream)
//
//    val want =
//      "3045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed"
//
//  }

//  it should "sec pub key" in {
//
//    val stream =
//      new ByteArrayInputStream(ByteVector.fromValidHex(txHex).toArray)
//    val tx = Transaction.parse(stream)
//    val want =
//      "0349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a"
//    assert(
//      tx.inputs.head.secPubKey().asInstanceOf[OP_PUSHDATA].data.toHex == want
//    )
//
//  }

  it should "serialize" in {

    val tx = Transaction.parse(txHex)
    val ser = tx.serialize
    assert(ser.toHex == txHex)
  }
//  it should "validate" in {}
//
//  it should "locktime" in {}
//
//  it should "isCoinbase" in {}
//
//  it should "serialize" in {}
//
//  it should "coinbaseHeight" in {}

//  it should "sig hash" in {
//
//    val txHex =
//      "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600"
//    val stream =
//      new ByteArrayInputStream(ByteVector.fromValidHex(txHex).toArray)
//    val tx = Transaction.parse(stream)
//    val hash_type = // ScriptElt.name2code(SIGHASH_ALL)
//    val want =
//      "27e0c5994dec7824e56dec6b2fcb342eb7cdb0d0957c2fce9882f715e85d81a6",
//
//    assert(tx.sigHash(0, hash_type) == want)
//
//  }

  it should "sign input" in {

    val stream =
      new ByteArrayInputStream(ByteVector.fromValidHex(txHex).toArray)
    val tx = Transaction.parse(stream)
    assert(tx.outputs.length == 2)
    val want = 32454049
    assert(tx.outputs.head.amount == want)
    val want1 = "76a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac"

    assert(tx.outputs.head.scriptPubKey.toHex == want1)
    val want2 = 10011545
    assert(tx.outputs(1).amount == want2)
    val want3 = "76a9141c4bc762dd5423e332166702cb75f40df79fea1288ac"
    assert(tx.outputs(1).scriptPubKey.toHex == want3)

  }

}

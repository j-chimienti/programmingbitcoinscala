package models

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import org.scalatest.FlatSpec
import scodec.bits.ByteVector

class TransactionTest extends FlatSpec {

  import HashHelper._

  behavior of "Transaction"

  val txHex =
    "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600"

  val tx: Transaction = Transaction.parse(txHex)

  it should "parse version and locktime" in {
    assert(tx.version == 1)
    assert(tx.locktime == 410393)
  }

  it should "parse inputs" in {
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

    assert(tx.outputs.length == 2)
    val want = 32454049
    assert(tx.outputs.head.amount == want)
    val want1 =
      "76a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac"
    val out = new ByteArrayOutputStream()
    writeScript(tx.outputs.head.scriptPubKey.toArray, out)
    val result = ByteVector(out.toByteArray)
    assert(result.toHex.drop(2) == want1)
    val want2 = 10011545
    assert(tx.outputs(1).amount == want2)
    val want3 =
      "76a9141c4bc762dd5423e332166702cb75f40df79fea1288ac"

    assert(tx.outputs(1).scriptPubKey.toHex == want3)

  }

  it should "der signature" in {

    val want =
      "3045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed"

    val der = tx.inputs.head.derSignature()
    val hashType = tx.inputs.head.hashType()

    assert(der.toHex == want)
    assert(hashType == SIGHASH_ALL)

  }

  it should "sec pub key" in {

    val want =
      "0349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a"
    assert(
      tx.inputs.head
        .secPubKey()
        .toHex == want
    )

  }

  it should "serialize" in {

    val ser = Transaction.serialize(tx)
    assert(ser.toHex == txHex)
  }

  it should "sign input" in {

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

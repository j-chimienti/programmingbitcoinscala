package models

import java.io.ByteArrayInputStream

import org.scalatest.{FlatSpec}
import scodec.bits.ByteVector

class TransactionTest extends FlatSpec {

  behavior of "Transaction"

  it should "parse version" in {

    val x_hex =
      "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600"

    val tx = Transaction.parse(x_hex)
    assert(tx.version == 1)
  }

  it should "parse inputs" in {

    val raw_tx =
      "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600"
    val stream =
      new ByteArrayInputStream(ByteVector.fromValidHex(raw_tx).toArray)
    val tx = Transaction.parse(stream)
    assert(tx.inputs.length == 1)
    val want =
      "d1c789a9c60383bf715f3f6ad9d14b91fe55f3deb369fe5d9280cb1a01793f81"
    assert(tx.inputs.head.prevTx.toHex == want)
    assert(tx.inputs.head.prevIdx == 0)
    val want2 =
      "483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a"
    //assert(tx.inputs.head.scriptSig.serialize == want2)
    assert(tx.inputs.head.sequence == 4294967295L)
  }

  it should "parse outputs outputs" in {

    val raw_tx =
      "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600"
    val tx = Transaction.parse(raw_tx)
    assert(tx.outputs.length == 2)
    val want = 32454049
    assert(tx.outputs.head.amount == want)
    val want1 =
      "76a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac"
    // assert(tx.outputs(0).scriptPubKey.serialize == want1)
    val want2 = 10011545
    assert(tx.outputs(1).amount == want2)
    val want3 =
      "76a9141c4bc762dd5423e332166702cb75f40df79fea1288ac"
    // assert(tx.outputs(1).scriptPubKey.serialize == want3)
  }

  it should "parse locktime" in {

    val rawTx =
      "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600"

    val stream =
      new ByteArrayInputStream(ByteVector.fromValidHex(rawTx).toArray)
    val tx = Transaction.parse(stream)
    assert(tx.locktime == 410393)
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

  it should "serialize" in {

    val rawTx =
      "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600"
    val rawTxStream =
      new ByteArrayInputStream(ByteVector.fromValidHex(rawTx).toArray)

    val tx = Transaction.parse(rawTxStream)

    assert(tx.serialize == rawTx)
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

}

package models

import models.HashHelper.p2pkh_script
import org.scalatest.AsyncFlatSpec
import scodec.bits.ByteVector
import services.TransactionService

class TransactionAsyncTest extends AsyncFlatSpec {

  behavior of "TransactionAsyncTest"

  val txHash =
    "d1c789a9c60383bf715f3f6ad9d14b91fe55f3deb369fe5d9280cb1a01793f81"

  val txHex =
    "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600"

  val tx: Transaction = Transaction.parse(txHex, testnet = false)

  it should "determine value" in {

    val index = 0
    val txValue = 42505594L
    val txIn =
      TxIn(txHash, index)
    for {
      value <- txIn.value()
    } yield {
      assert(value == txValue)
    }

  }

  it should "determine input public key" in {

    val index = 0
    val txIn = TxIn(txHash, index)
    val want =
      "76a914a802fc56c704ce87c42d7c92eb75e7896bdc41ae88ac"
    for {
      scriptPubKey <- txIn.scriptPubKey()
    } yield assert(scriptPubKey.toHex == want)

  }

  it should "determine fee" in {

    val want1 = 40000

    for {
      fee1 <- tx.fee
    } yield {
      assert(fee1 == want1)
    }
  }

  it should "sig hash" in {

    val want =
      "27e0c5994dec7824e56dec6b2fcb342eb7cdb0d0957c2fce9882f715e85d81a6"
    for {
      result <- tx.sigHash(0, SIGHASH_ALL)
    } yield {

      assert(result.toHex == want)
    }

  }

  it should "verify input" in {

    for {
      result <- tx.verifyInput(0)

    } yield {
      assert(result)
    }
  }

  it should "sign input" in {

    val private_key = PrivateKey(BigInt(8675309))
    val txIns = Seq(
      TxIn(
        "0025bc3c0fa8b7eb55b9437fdbd016870d18e0df0ace7bc9864efc38414147c8",
        0
      )
    )
    val h160 = ByteVector.fromValidBase58("mzx5YhAH9kNHtcN481u6WkjeHjYtVeKVh2")
    val out1 =
      TxOut(
        amount = (BigDecimal(0.99) * 100000000).toLong,
        scriptPubKey = p2pkh_script(h160)
      )

    val h160__ =
      ByteVector.fromValidBase58("mnrVtF8DWjMu839VW3rBfgYaAfKk8983Xf")
    val out2 =
      TxOut(
        amount = (BigDecimal(0.1) * 100000000).toLong,
        scriptPubKey = p2pkh_script(h160__)
      )
    val tx_outs = Seq(out1, out2)
    val tx =
      Transaction(
        version = 1,
        inputs = txIns,
        outputs = tx_outs,
        locktime = 0,
        testnet = true
      )

    val SIGHASH_ALL = 1
    for {

      result <- tx
        .signInput(0, private_key, SIGHASH_ALL)
        .flatMap(response => response.map(res => res))
    } yield {
      assert(result)
    }
  }

}

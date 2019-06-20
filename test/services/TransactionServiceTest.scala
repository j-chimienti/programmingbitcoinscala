package services

import fr.acinq.bitcoin.Base58Check
import models._
import org.scalatest.concurrent.{AsyncAssertions, Waiters}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.play.PlaySpec

import scala.language.postfixOps
import org.scalatest.time.SpanSugar._
import scodec.bits.ByteVector

import scala.concurrent.ExecutionContext.Implicits.global

class TransactionServiceTest
    extends PlaySpec
    with GuiceOneAppPerSuite
    with Waiters {

  val txService: TransactionService =
    app.injector.instanceOf(classOf[TransactionService])

  "fixme: first test always fails w/ async calls" in {

    val w = new Waiter

    val hexStr =
      "d1c789a9c60383bf715f3f6ad9d14b91fe55f3deb369fe5d9280cb1a01793f81"

    for {
      tx <- txService.fetch(hexStr)
    } yield {

      w {
        assert(tx.version == 12)
        assert(tx.locktime == 0)

        assert(tx.txIn.length == 2)
        assert(
          tx.txIn.head.prevTxId.toHex == "cbf43825e0b92ba3bfabaec509e14ee9132df1e92ffdfc6636f848fbf0537c13"
        )
        assert(tx.txIn.head.prevIdx == 0)
        //assert(value == 17608602)
        assert(tx.txIn.head.sequence == 4294967295L)

        assert(tx.txOut.length == 2)
        assert(tx.txOut.head.amount == 42505594L)

      }
      w.dismiss()

    }
    w.await(timeout(1 seconds))
  }

  "should fetch tx" in {

    val w = new Waiter

    val hexStr =
      "d1c789a9c60383bf715f3f6ad9d14b91fe55f3deb369fe5d9280cb1a01793f81"

    for {
      tx <- txService.fetch(hexStr)
      //fee <- txService.fee(tx, false) // fee is a seperate call
      //value <- service.value(txHash, 0, false)
    } yield {

      w {
        assert(tx.version == 1)
        assert(tx.locktime == 0)
        //assert(fee == 8800)

        assert(tx.txIn.length == 2)
        assert(
          tx.txIn.head.prevTxId.toHex == "cbf43825e0b92ba3bfabaec509e14ee9132df1e92ffdfc6636f848fbf0537c13"
        )
        assert(tx.txIn.head.prevIdx == 0)
        //assert(value == 17608602)
        assert(tx.txIn.head.sequence == 4294967295L)

        assert(tx.txOut.length == 2)
        assert(tx.txOut.head.amount == 42505594L)

      }
      w.dismiss()

    }
    w.await(timeout(5 seconds))
  }

  val txHash =
    "d1c789a9c60383bf715f3f6ad9d14b91fe55f3deb369fe5d9280cb1a01793f81"
  val txHex =
    "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600"
  val tx: Transaction = Transaction.parse(txHex, testnet = false)

  "determine value" in {

    val waiter = new Waiter
    val index = 0
    val txValue = 42505594L
    for {
      value <- txService.value(txHash, index, tx.testnet)
    } yield {
      waiter {
        assert(value == txValue)
      }
      waiter.dismiss()
    }
    waiter.await(timeout(5 seconds))
  }

  "determine input public key" in {

    val waiter = new Waiter

    val index = 0
    val want =
      "76a914a802fc56c704ce87c42d7c92eb75e7896bdc41ae88ac"
    for {
      scriptPubKey <- txService.scriptPubKey(txHash, index, false) // txIn.scriptPubKey()
    } yield {

      waiter {
        assert(scriptPubKey.toHex == want)
      }
      waiter.dismiss()
    }

    waiter.await(timeout(5 seconds))
  }

  "determine fee" in {

    val waiter = new Waiter
    val want1 = 40000
    for {
      fee1 <- txService.fee(tx)
    } yield {
      waiter {
        assert(fee1 == want1)
      }
      waiter.dismiss()
    }
    waiter.await(timeout(15 seconds))
  }

  "sig hash" in {

    val waiter = new Waiter

    val want =
      "27e0c5994dec7824e56dec6b2fcb342eb7cdb0d0957c2fce9882f715e85d81a6"
    for {
      result <- txService.sigHash(tx, 0, SIGHASH_ALL)
    } yield {

      waiter {

        assert(result.toHex == want)
        waiter.dismiss()
      }
      waiter.await(timeout(5 seconds))
    }

  }

  "verify tx" in {

    val waiter = new Waiter
    txService
      .verify(tx)
      .flatMap(result => result)
      .map(isVerified => {
        waiter {
          assert(isVerified)
          waiter.dismiss()
        }
      })
    waiter.await(timeout(5 seconds))
  }

  "sign input" in {

    val waiter = new Waiter

    val privateKey = PrivateKey(8675309)
    val txIns = Seq(
      TxIn(
        "0025bc3c0fa8b7eb55b9437fdbd016870d18e0df0ace7bc9864efc38414147c8",
        0,
        ByteVector.empty,
        0xffffffff
      )
    )
    val out1 =
      TxOut(
        amount = (BigDecimal(0.99) * 100000000).toLong,
        scriptPubKey = OP_DUP :: OP_HASH160 :: OP_PUSHDATA(
          Base58Check.decode("mzx5YhAH9kNHtcN481u6WkjeHjYtVeKVh2")._2
        ) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil
      )

    val out2 =
      TxOut(
        amount = (BigDecimal(0.1) * 100000000).toLong,
        scriptPubKey = OP_DUP :: OP_HASH160 :: OP_PUSHDATA(
          Base58Check.decode("mnrVtF8DWjMu839VW3rBfgYaAfKk8983Xf")._2
        )
          :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil
      )
    val tx_outs = Seq(out1, out2)
    val tx =
      Transaction(
        version = 1,
        txIn = txIns,
        txOut = tx_outs,
        locktime = 0,
        testnet = true
      )

    val SIGHASH_ALL = 1

    txService
      .signInput(tx, 0, privateKey, SIGHASH_ALL)
      .map(tx => {

        waiter {
          assert(true)
          waiter.dismiss()
        }

      })

    waiter.await(timeout(5 seconds))
  }

  "create testnet tx" in {

    val waiter = new Waiter

    val key =
      "100101670600421896928058468371021333732585646060773712455132778496481965499204"
    val key2 =
      "113034659463690134968056054716646373354929880089725093972680027214452886864239"

    val to = "mwJn1YPMq7y5F8J3LkC5Hxg9PHyZ5K4cFv"

    val pk = PrivateKey(BigInt(key))

    // mwoUNPQkCfNHaRnkcCHUMgbHvXWaAELWRG
    val addr = pk.publicKey.address(compressed = true, testnet = true)

    val txId =
      "0c216789ab3266438f6e977d450a19eb6c983f73905fe9bed4eb9e37fa592159"

    val pk2 = PrivateKey(BigInt(key2))
    // n4Mdcq3x2Fzt6HekzzDp5tedv8yavhVhLy
    val changeAddr = pk2.publicKey.address(true, true)

    val input =
      TxIn(txId, 0, ByteVector.empty, 0xffffffff)

    val foo: Seq[ScriptElt] = Script.pay2pkh(to)
    val bar: Seq[ScriptElt] = Script.pay2pkh(changeAddr)

    val value = (0.01 * 1e8).toInt
    val sendAmt = (0.6 * value).toInt
    val changeAmt = value - sendAmt - 50000
    val outputs = Seq(
      TxOut(amount = sendAmt, scriptPubKey = foo),
      TxOut(amount = changeAmt, scriptPubKey = bar)
    )

    val tx = Transaction(
      version = 1,
      txIn = Seq(input),
      txOut = outputs,
      locktime = 0L,
      testnet = true
    )

    txService
      .signInput(tx, 0, pk, SIGHASH_ALL)
      .map(tx1 => {
        val j = tx1

        waiter {
          assert(true)
          waiter.dismiss()
        }
      })

    waiter.await(timeout(5 seconds))
  }

}

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

class BlockchainServiceTest
    extends PlaySpec
    with GuiceOneAppPerSuite
    with Waiters {

  val txService: BlockchainService =
    app.injector.instanceOf(classOf[BlockchainService])

  val txId =
    "d1c789a9c60383bf715f3f6ad9d14b91fe55f3deb369fe5d9280cb1a01793f81"
  val txHex =
    "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600"
  val tx: Transaction = Transaction.parse(txHex, testnet = false)

  "fixme: first test always fails w/ async calls" in {

    val w = new Waiter

    for {
      tx <- txService.fetch(txId)
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

    for {
      tx <- txService.fetch(txId)
      //fee <- txService.fee(tx, false) // fee is a seperate call
      //value <- service.value(txId, 0, false)
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

  "determine value" in {

    val waiter = new Waiter
    val index = 0
    val txValue = 42505594L
    for {
      value <- txService.value(txId, index, tx.testnet)
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
      scriptPubKey <- txService.scriptPubKey(txId, index, false) // txIn.scriptPubKey()
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
    val want2 = 140500
    val tx2 = Transaction.parse(
      "010000000456919960ac691763688d3d3bcea9ad6ecaf875df5339e148a1fc61c6ed7a069e010000006a47304402204585bcdef85e6b1c6af5c2669d4830ff86e42dd205c0e089bc2a821657e951c002201024a10366077f87d6bce1f7100ad8cfa8a064b39d4e8fe4ea13a7b71aa8180f012102f0da57e85eec2934a82a585ea337ce2f4998b50ae699dd79f5880e253dafafb7feffffffeb8f51f4038dc17e6313cf831d4f02281c2a468bde0fafd37f1bf882729e7fd3000000006a47304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a7160121035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937feffffff567bf40595119d1bb8a3037c356efd56170b64cbcc160fb028fa10704b45d775000000006a47304402204c7c7818424c7f7911da6cddc59655a70af1cb5eaf17c69dadbfc74ffa0b662f02207599e08bc8023693ad4e9527dc42c34210f7a7d1d1ddfc8492b654a11e7620a0012102158b46fbdff65d0172b7989aec8850aa0dae49abfb84c81ae6e5b251a58ace5cfeffffffd63a5e6c16e620f86f375925b21cabaf736c779f88fd04dcad51d26690f7f345010000006a47304402200633ea0d3314bea0d95b3cd8dadb2ef79ea8331ffe1e61f762c0f6daea0fabde022029f23b3e9c30f080446150b23852028751635dcee2be669c2a1686a4b5edf304012103ffd6f4a67e94aba353a00882e563ff2722eb4cff0ad6006e86ee20dfe7520d55feffffff0251430f00000000001976a914ab0c0b2e98b1ab6dbf67d4750b0a56244948a87988ac005a6202000000001976a9143c82d7df364eb6c75be8c80df2b3eda8db57397088ac46430600"
    )
    for {
      fee1 <- txService.fee(tx)
      fee2 <- txService.fee(tx2)
    } yield {
      waiter {
        assert(fee1 == want1)
        assert(fee2 == want2)
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
        val z = result._2
        assert(z.toHex == want)
        waiter.dismiss()
      }
      waiter.await(timeout(5 seconds))
    }

  }

  "verify p2pkh" in {

    val waiter = new Waiter
    val h1 = "452c629d67e41baec3ac6f04fe744b4b9617f8f859c63b3002f8684e7a4fee03"
    val h2 = "452c629d67e41baec3ac6f04fe744b4b9617f8f859c63b3002f8684e7a4fee03"
    for {

      tx1 <- txService.fetch(h1, false)
      tx2 <- txService.fetch(h2, true)
      v1 <- txService.verify(tx1).flatMap(result => result)
      v2 <- txService.verify(tx2).flatMap(r => r)
    } yield {

      waiter {
        assert(v1)
        assert(v2)
        waiter.dismiss()
      }
    }
    waiter.await(timeout(5 seconds))
  }

  "verify p2sh" in {

    val waiter = new Waiter
    val h1 = "46df1a9484d0a81d03ce0ee543ab6e1a23ed06175c104a178268fad381216c2b"
    for {

      tx1 <- txService.fetch(h1, false)
      v1 <- txService.verify(tx1).flatMap(result => result)
    } yield {

      waiter {
        assert(v1)
        waiter.dismiss()
      }
    }
    waiter.await(timeout(5 seconds))
  }

  "sign input" in {

    val waiter = new Waiter

    val privateKey = PrivateKey(8675309)
    val tx = Transaction.parse(
      "010000000199a24308080ab26e6fb65c4eccfadf76749bb5bfa8cb08f291320b3c21e56f0d0d00000000ffffffff02408af701000000001976a914d52ad7ca9b3d096a38e752c2018e6fbc40cdf26f88ac80969800000000001976a914507b27411ccf7f16f10297de6cef3f291623eddf88ac00000000",
      true
    )
    val want =
      "010000000199a24308080ab26e6fb65c4eccfadf76749bb5bfa8cb08f291320b3c21e56f0d0d0000006b4830450221008ed46aa2cf12d6d81065bfabe903670165b538f65ee9a3385e6327d80c66d3b502203124f804410527497329ec4715e18558082d489b218677bd029e7fa306a72236012103935581e52c354cd2f484fe8ed83af7a3097005b2f9c60bff71d35bd795f54b67ffffffff02408af701000000001976a914d52ad7ca9b3d096a38e752c2018e6fbc40cdf26f88ac80969800000000001976a914507b27411ccf7f16f10297de6cef3f291623eddf88ac00000000"

    txService
      .signInput(tx, 0, privateKey, SIGHASH_ALL)
      .flatMap(tx => tx)
      .map(txResult => {
        waiter {
          assert(txResult.serialize.toHex == want)
          waiter.dismiss()
        }
      })

    waiter.await(timeout(10 seconds))
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

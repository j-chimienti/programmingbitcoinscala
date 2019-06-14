package models

import java.io.ByteArrayOutputStream

import fr.acinq.bitcoin.ByteVector32
import org.scalatest.AsyncFlatSpec
import scodec.bits.ByteVector

class TransactionAsyncTest extends AsyncFlatSpec {

  behavior of "TransactionAsyncTest"

  it should "determine value" in {

    val txHash =
      "d1c789a9c60383bf715f3f6ad9d14b91fe55f3deb369fe5d9280cb1a01793f81"
    val index = 0
    val sequence = 0
    val want = 42505594L
    val txIn =
      TxIn(txHash, index, sequence)
    for {
      value <- txIn.value()
    } yield {
      assert(value.getOrElse(0L) == want)
    }

  }

  it should "determine fee" in {

    val raw_tx =
      "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600"
    val tx = Transaction.parse(raw_tx)
    val want1 = 40000

    val raw2 =
      "010000000456919960ac691763688d3d3bcea9ad6ecaf875df5339e148a1fc61c6ed7a069e010000006a47304402204585bcdef85e6b1c6af5c2669d4830ff86e42dd205c0e089bc2a821657e951c002201024a10366077f87d6bce1f7100ad8cfa8a064b39d4e8fe4ea13a7b71aa8180f012102f0da57e85eec2934a82a585ea337ce2f4998b50ae699dd79f5880e253dafafb7feffffffeb8f51f4038dc17e6313cf831d4f02281c2a468bde0fafd37f1bf882729e7fd3000000006a47304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a7160121035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937feffffff567bf40595119d1bb8a3037c356efd56170b64cbcc160fb028fa10704b45d775000000006a47304402204c7c7818424c7f7911da6cddc59655a70af1cb5eaf17c69dadbfc74ffa0b662f02207599e08bc8023693ad4e9527dc42c34210f7a7d1d1ddfc8492b654a11e7620a0012102158b46fbdff65d0172b7989aec8850aa0dae49abfb84c81ae6e5b251a58ace5cfeffffffd63a5e6c16e620f86f375925b21cabaf736c779f88fd04dcad51d26690f7f345010000006a47304402200633ea0d3314bea0d95b3cd8dadb2ef79ea8331ffe1e61f762c0f6daea0fabde022029f23b3e9c30f080446150b23852028751635dcee2be669c2a1686a4b5edf304012103ffd6f4a67e94aba353a00882e563ff2722eb4cff0ad6006e86ee20dfe7520d55feffffff0251430f00000000001976a914ab0c0b2e98b1ab6dbf67d4750b0a56244948a87988ac005a6202000000001976a9143c82d7df364eb6c75be8c80df2b3eda8db57397088ac46430600"
    val tx2 = Transaction.parse(raw2)
    val want2 = 140500

    for {
      fee1 <- tx.fee()
      fee2 <- tx2.fee()
    } yield {
      assert(fee1 == want1)
      assert(fee2 == want2)
    }
  }

  it should "determine input public key" in {

    val tx_hash =
      "d1c789a9c60383bf715f3f6ad9d14b91fe55f3deb369fe5d9280cb1a01793f81"
    val index = 0
    val txIn = TxIn(
      prevTx = ByteVector32(ByteVector.fromValidHex(tx_hash)),
      prevIdx = index,
      scriptSig = ByteVector.empty,
      sequence = 0L
    )
    val want =
      "76a914a802fc56c704ce87c42d7c92eb75e7896bdc41ae88ac"
    for {

      scriptPubKey <- txIn.scriptPubKey().map(r => r.get)
    } yield {

      assert(scriptPubKey.toHex == want)
    }

  }
}

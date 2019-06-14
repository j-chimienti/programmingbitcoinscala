package services

import akka.http.scaladsl.util.FastFuture
import org.scalatest.AsyncFlatSpec

import scala.concurrent.ExecutionContext.Implicits.global

class TransactionServiceTest extends AsyncFlatSpec {

  behavior of "TransactionService"

  it should "Read tx from http request" in {

    val hexStr =
      "d1c789a9c60383bf715f3f6ad9d14b91fe55f3deb369fe5d9280cb1a01793f81"

    for {
      tx <- TransactionService.fetch(hexStr).map(result => result.get)
      fee <- tx.fee() // fee is a seperate call
      value <- tx.inputs.head.value()
    } yield {

      assert(tx.version == 1)
      assert(tx.locktime == 0)
      assert(fee == 8800)

      assert(tx.inputs.length == 2)
      assert(
        tx.inputs.head.txId.toHex == "cbf43825e0b92ba3bfabaec509e14ee9132df1e92ffdfc6636f848fbf0537c13"
      )
      assert(tx.inputs.head.prevIdx == 0)
      assert(value.get == 17608602)
      assert(tx.inputs.head.sequence == 4294967295L)

      assert(tx.outputs.length == 2)
      assert(tx.outputs.head.amount == 42505594L)

    }
  }

}

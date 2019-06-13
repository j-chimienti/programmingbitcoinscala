package services

import org.scalatest.{AsyncFlatSpec, FlatSpec}

import scala.concurrent.ExecutionContext.Implicits.global

class TransactionServiceTest extends AsyncFlatSpec {

  behavior of "TransactionService"
  val txService = TransactionService

  it should "fetch Transaction" in {

    val hexStr =
      "d1c789a9c60383bf715f3f6ad9d14b91fe55f3deb369fe5d9280cb1a01793f81"

    for {

      tx <- txService.fetch(hexStr, false)
      fee <- tx.fee(false)
    } yield {

      assert(tx.version == 1)
      assert(tx.locktime == 0)
      assert(fee == 8800)
    }
  }

}

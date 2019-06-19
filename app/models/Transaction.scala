package models

import java.io.{ByteArrayInputStream, InputStream, OutputStream}

import scodec.bits.ByteVector
import services.TransactionService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  *
  * @param version Tranaction data format version
  * @param inputs Transaction inputs
  * @param outputs Transaction outputs
  * @param locktime the block number or timestamp at which this transaction is locked
  */
case class Transaction(version: Long,
                       inputs: Seq[TxIn],
                       outputs: Seq[TxOut],
                       locktime: Long,
                       testnet: Boolean = false)
    extends BtcSerializable[Transaction] {

  override def serializer: BtcSerializer[Transaction] = Transaction

  def serialize = Transaction.serialize(this)

  override def toString: String = {
    var tx_ins = ""
    for (tx_in <- tx_ins) tx_ins += tx_in.toString + "\n"
    var tx_outs = ""
    for (tx_out <- tx_outs) tx_outs += tx_out.toString + "\n"
    s"version: $version\ntx_ins:\n$inputs\ntx_outs:\n$outputs\nlocktime: $locktime\n"
  }

  val PROTOCOL_VERSION = 70015

  def fee: Future[Long] = Transaction.fee(this, testnet)

  def sigHash(index: Int, hashType: Int) =
    Transaction.sigHash(this, index, hashType)

  def verifyInput(index: Int): Future[Boolean] =
    Transaction.verifyInput(this, index)

  def signInput(index: Int, privateKey: PrivateKey, hashType: Int) =
    Transaction
      .signInput(this, index, privateKey, hashType)
      .flatMap(tx => tx)
}

object Transaction extends BtcSerializer[Transaction] {

  import HashHelper._

  // if lockTime >= LOCKTIME_THRESHOLD it is a unix timestamp otherwise it is a block height
  val LOCKTIME_THRESHOLD = 500000000L

  def parse(txId: String, testnet: Boolean): Transaction =
    parse(txId).copy(testnet = testnet)

  def parse(hexStr: String): Transaction = {
    val stream = new ByteArrayInputStream(
      ByteVector.fromValidHex(hexStr).toArray
    )
    parse(stream)
  }
  def parse(stream: InputStream): Transaction = {
    val version = uint32(stream)
    val inputsCount = readVarint(stream)
    val inputs: Seq[TxIn] = for (_ <- 1L to inputsCount)
      yield TxIn.parse(stream)
    val outputsCount = readVarint(stream)
    val outputs: Seq[TxOut] = for (_ <- 1L to outputsCount)
      yield TxOut.parse(stream)
    val locktime = uint32(stream)
    Transaction(version.toInt, inputs, outputs, locktime)
  }

  def fromId(id: String, testnet: Boolean): Future[Transaction] =
    TransactionService.fetch(id, testnet)

  def serialize(tx: Transaction, out: OutputStream): Unit = {
    writeUInt32(tx.version.toInt, out)
    writeVarint(tx.inputs.length, out)
    for (input <- tx.inputs) TxIn.serialize(input, out)
    writeVarint(tx.outputs.length, out)
    for (output <- tx.outputs) TxOut.serialize(output, out)
    writeUInt32(tx.locktime.toInt, out)

  }

  def isCoinbase(tx: Transaction): Boolean =
    tx.inputs.length == 1 && tx.inputs.head.prevIdx == 0xffffffff

  override def validate(tx: Transaction): Unit = {

    require(tx.inputs.nonEmpty, "Inputs cannot be empty")
    require(tx.outputs.nonEmpty, "Outputs cannot be empty")
    require(
      tx.outputs.map(_.amount).sum < MaxMoney,
      "Sum of outputs amount is invalid"
    )
    val outPoints = tx.inputs.map(_.prevTx.toHex)
    require(outPoints.size == outPoints.toSet.size, "Duplicate inputs")

  }

  /**
    *   Fee of the tx in satoshi
    * @param testnet
    * @return
    */
  def fee(tx: Transaction, testnet: Boolean = false): Future[Long] = {

    val list = tx.inputs.map(input => input.value(testnet))
    for {
      inputsFound <- Future.sequence(list)

    } yield {
      if (tx.inputs.length != inputsFound.length)
        throw new RuntimeException(s"Error fetching tx fee")
      val inputsSum = inputsFound.sum
      val outputsSum = tx.outputs.map(_.amount).sum
      inputsSum - outputsSum
    }
  }

  /**
    *
    * @param index
    * @param hashType
    * @return the integer of the hash that needs to get signed for index input_index
    */
  def sigHash(tx: Transaction,
              index: Int,
              hashType: Int): Future[ByteVector] = {

    val altTxIns = for (txIn <- tx.inputs)
      yield txIn.copy(scriptSig = ByteVector.empty)
    val input = altTxIns(index)
    for {
      scriptPubKey <- input.scriptPubKey(tx.testnet)
    } yield {
      val txIns =
        altTxIns.updated(index, input.copy(scriptSig = scriptPubKey))
      val tx1 = tx.copy(inputs = txIns)
      val result = Transaction.serialize(tx1) ++ writeUInt32(hashType)
      val hash = hash256(result)
      hash

    }
  }

  def verifyInput(transaction: Transaction, index: Int): Future[Boolean] = {

    val tx = transaction.inputs(index)
    val pubKey = tx.secPubKey()
    val point = S256Point.parse(pubKey)
    val der = tx.derSignature()
    val signature = Signature.parse(der)
    val ht = tx.hashType()
    sigHash(transaction, index, ht)
      .map(z => {

        val foo = point.verify(z, signature)
        foo
      })

  }

  /** fixme
    * Sign the input w/ the Private Key
    * @param index
    * @param privateKey
    * @param hashType
    * @return
    */
  def signInput(tx: Transaction,
                index: Int,
                privateKey: PrivateKey,
                hashType: Int): Future[Future[Transaction]] = {

    sigHash(tx, index, hashType).map(
      z => {
        val signature = privateKey.sign(z)
        val der = signature.der
        val sig = ByteVector(der :+ hashType.toByte)
        val publicKey = privateKey.publicKey.sec(compressed = true)
        val scriptSig: Seq[ScriptElt] = OP_PUSHDATA(sig) :: OP_PUSHDATA(
          publicKey
        ) :: Nil
        val ss = Script.serialize(scriptSig)
        val tx1 = tx.copy(
          inputs = tx.inputs
            .updated(index, tx.inputs(index).copy(scriptSig = ss))
        )

        for {

          isVerified <- verifyInput(tx1, index)
        } yield {

          require(isVerified)

          tx1
        }

      }
    )
  }

}

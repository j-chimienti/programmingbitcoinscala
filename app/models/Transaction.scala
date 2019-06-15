package models

import scodec.bits.ByteVector
import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  InputStream,
  OutputStream
}

import HashHelper._
import fr.acinq.bitcoin.ByteVector32
import fr.acinq.bitcoin.Protocol.{writeCollection, writeUInt32}

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
                       var inputs: Seq[TxIn],
                       outputs: Seq[TxOut],
                       locktime: Long,
                       testnet: Boolean = false) {

  def validate = {

    require(inputs.nonEmpty, "Inputs cannot be empty")
    require(outputs.nonEmpty, "Outputs cannot be empty")
    require(
      outputs.map(_.amount).sum < MaxMoney,
      "Sum of outputs amount is invalid"
    )
    val outPoints = inputs.map(_.prevTx.toHex)
    require(outPoints.size == outPoints.toSet.size, "Duplicate inputs")

  }

  override def toString: String = {
    var tx_ins = ""
    for (tx_in <- tx_ins) tx_ins += tx_in.toString + "\n"
    var tx_outs = ""
    for (tx_out <- tx_outs) tx_outs += tx_out.toString + "\n"
    s"version: $version\ntx_ins:\n$inputs\ntx_outs:\n$outputs\nlocktime: $locktime\n"
  }

  def isCoinbase: Boolean =
    inputs.length == 1 && inputs.head.prevIdx == 0xffffffff

  val PROTOCOL_VERSION = 70015
  def serialize(out: OutputStream): Unit = {

    writeUInt32(version.toInt, out)
    writeVarint(inputs.length, out)
    for (input <- inputs) input.serialize(out)
    writeVarint(outputs.length, out)
    for (output <- outputs) output.serialize(out)
    writeUInt32(locktime.toInt, out)

  }

  def serialize: ByteVector = {

    val out = new ByteArrayOutputStream()
    serialize(out)
    ByteVector.view(out.toByteArray)

  }

  /**
    *   Fee of the tx in satoshi
    * @param testnet
    * @return
    */
  def fee(testnet: Boolean = false): Future[Long] = {

    val list = inputs.map(input => input.value(testnet))
    for {
      inputsRaw <- Future.sequence(list)

    } yield {
      val inputsFound = inputsRaw.flatten
      if (inputs.length != inputsFound.length)
        throw new RuntimeException(s"Error fetching tx fee")

      val inputsSum = inputsFound.sum
      val outputsSum = outputs.map(_.amount).sum
      inputsSum - outputsSum
    }
  }

  /**
    *
    * @param index
    * @param hashType
    * @return the integer of the hash that needs to get signed for index input_index
    */
  def sigHash(index: Int, hashType: Int): Future[ByteVector32] = {

    val altTxIns = for (txIn <- inputs)
      yield TxIn(txIn.prevTx, txIn.prevIdx, ByteVector.empty, txIn.sequence)

    val signingInput = altTxIns(index)
    for {
      responseOpt <- signingInput.scriptPubKey(testnet)
    } yield {
      responseOpt match {
        case None =>
          println("ERROR fetching scriptPubKey")
          throw new RuntimeException("Error")
        case Some(scriptPubkey) =>
          // todo: check wheter edit idx of tx in
          val txIns = altTxIns
            .slice(0, index)
            .:+(altTxIns(index).copy(scriptSig = scriptPubkey)) ++ altTxIns
            .slice(index + 1, altTxIns.length)
          val altTx = Transaction(version, txIns, outputs, locktime)
          val result = altTx.serialize ++ writeUInt32(hashType)
          val s256 = hash256(result)
          s256
      }
    }
  }

  def verifyInput(index: Int): Future[Boolean] = {

    val tx = inputs(index)
    val s = Script.write(Seq(tx.secPubKey()))

    val point = S256Point.parse(s)
    val signature = Signature.parse(tx.derSignature())
    val ht = tx.hashType()

    for { z <- sigHash(index, ht) } yield
      point.verify(BigInt(z.toHex, 16), signature)

    /*
       # get the relevant input
        tx_in = self.tx_ins[input_index]
        # parse the point from the sec format (tx_in.sec_pubkey())
        point = S256Point.parse(tx_in.sec_pubkey())
        # parse the signature from the der format (tx_in.der_signature())
        signature = Signature.parse(tx_in.der_signature())
        # get the hash type from the input (tx_in.hash_type())
        hash_type = tx_in.hash_type()
        # get the sig_hash (z)
        z = self.sig_hash(input_index, hash_type)
        # use point.verify on the z and signature
        return point.verify(z, signature)
   */
  }

  /**
    * Sign the input w/ the Private Key
    * @param index
    * @param privateKey
    * @param hashType
    * @return
    */
  def signInput(index: Int,
                privateKey: PrivateKey,
                hashType: Int): Future[Future[Boolean]] = {

    for {
      z <- sigHash(index, hashType)
    } yield {
      val i = BigInt(z.toHex, 16)
      val der = privateKey.sign(i).der
      val sig = der.:+(hashType.toByte)
      val sec = privateKey.publicKey.sec()
      val script = Script(sig ++ sec.toArray)
      val scriptSig = Script.write(script.elements)
      val updated = inputs(index).copy(scriptSig = scriptSig)
      inputs = inputs.slice(0, index).:+(updated) ++ inputs.slice(
        index + 1,
        inputs.length
      )
      for {
        verified <- verifyInput(index)
      } yield verified
    }
  }

//  def coinbaseHeight =
//    HashHelper.littleEndianToInt(inputs.head.scriptPubKey)

}

object Transaction {

  // if lockTime >= LOCKTIME_THRESHOLD it is a unix timestamp otherwise it is a block height
  val LOCKTIME_THRESHOLD = 500000000L

  def parse(hexStr: String): Transaction = {

    val stream = new ByteArrayInputStream(
      ByteVector.fromValidHex(hexStr).toArray
    )
    parse(stream)
  }
  def parse(stream: InputStream): Transaction = {

    val version = uint32(stream)
    val numInputs = readVarint(stream)
    val inputs: Seq[TxIn] = for (_ <- 1L to numInputs)
      yield TxIn.parse(stream)
    val numOutputs = readVarint(stream)
    val outputs: Seq[TxOut] = for (_ <- 1L to numOutputs)
      yield TxOut.parse(stream)
    val locktime = uint32(stream)
    Transaction(version.toInt, inputs, outputs, locktime)
  }
}

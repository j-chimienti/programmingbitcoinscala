package models

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  InputStream,
  OutputStream
}

import scodec.bits.ByteVector

trait BtcSerializable[T] {
  def serializer: BtcSerializer[T]
}

trait BtcSerializer[T] {

  def parse(data: InputStream): T
  def parse(data: ByteVector): T = parse(new ByteArrayInputStream(data.toArray))

  def serialize(t: T, out: OutputStream): Unit

  def serialize(t: T): ByteVector = {
    val outputStream = new ByteArrayOutputStream()
    serialize(t, outputStream)
    ByteVector.view(outputStream.toByteArray)
  }

  def validate(t: T): Unit = {}
}

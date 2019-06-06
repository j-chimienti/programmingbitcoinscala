package models

/**
  * Utility class for doing byte swapping (i.e. conversion between
  * little-endian and big-endian representations) of different data types.
  * Byte swapping is typically used when data is read from a stream
  * delivered by a system of different endian type as the present one.
  *
  * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
  */
object ByteSwapper {
  /**
    * Byte swap a single short value.
    *
    * @param value Value to byte swap.
    * @return Byte swapped representation.
    */
  def swap(value: Short): Short = {
    val b1 = value & 0xff
    val b2 = (value >> 8) & 0xff
    (b1 << 8 | b2 << 0).toShort
  }

  /**
    * Byte swap a single int value.
    *
    * @param value Value to byte swap.
    * @return Byte swapped representation.
    */
  def swap(value: Int): Int = {
    val b1 = (value >> 0) & 0xff
    val b2 = (value >> 8) & 0xff
    val b3 = (value >> 16) & 0xff
    val b4 = (value >> 24) & 0xff
    b1 << 24 | b2 << 16 | b3 << 8 | b4 << 0
  }

  /**
    * Byte swap a single long value.
    *
    * @param value Value to byte swap.
    * @return Byte swapped representation.
    */
  def swap(value: Long): Long = {
    val b1 = (value >> 0) & 0xff
    val b2 = (value >> 8) & 0xff
    val b3 = (value >> 16) & 0xff
    val b4 = (value >> 24) & 0xff
    val b5 = (value >> 32) & 0xff
    val b6 = (value >> 40) & 0xff
    val b7 = (value >> 48) & 0xff
    val b8 = (value >> 56) & 0xff
    b1 << 56 | b2 << 48 | b3 << 40 | b4 << 32 | b5 << 24 | b6 << 16 | b7 << 8 | b8 << 0
  }

  /**
    * Byte swap a single float value.
    *
    * @param value Value to byte swap.
    * @return Byte swapped representation.
    */
  def swap(value: Float): Float = {
    var intValue = java.lang.Float.floatToIntBits(value)
    intValue = swap(intValue)
    java.lang.Float.intBitsToFloat(intValue)
  }

  /**
    * Byte swap a single double value.
    *
    * @param value Value to byte swap.
    * @return Byte swapped representation.
    */
  def swap(value: Double): Double = {

    var longValue: Long = java.lang.Double.doubleToLongBits(value)
    longValue = swap(longValue)
    java.lang.Double.longBitsToDouble(longValue)
  }

  /**
    * Byte swap an array of shorts. The result of the swapping
    * is put back into the specified array.
    *
    * @param array Array of values to swap
    */
  def swap(array: Array[Short]): Array[Short] = {

//    for (int i = 0; i < array.length; i++)
//    array[i] = swap (array[i]);


    for (foo <- array.indices) {
      array(foo) = swap(array(foo))
    }
    array
  }

  /**
    * Byte swap an array of ints. The result of the swapping
    * is put back into the specified array.
    *
    * @param array Array of values to swap
    */
  def swap(array: Array[Int]): Array[Int] = {
    for (foo <- array.indices) {
      val i : Int = array(foo)
      array(foo) = swap(i)
    }
    array
  }

  /**
    * Byte swap a single int value.
    *
    * @param value Value to byte swap.
    * @return Byte swapped representation.
    */
  //  def swap(value: Int): Int = {
  //    val b1 = (value >> 0) & 0xff
  //    val b2 = (value >> 8) & 0xff
  //    val b3 = (value >> 16) & 0xff
  //    val b4 = (value >> 24) & 0xff
  //    b1 << 24 | b2 << 16 | b3 << 8 | b4 << 0
  //  }


  /**
    * Byte swap an array of longs. The result of the swapping
    * is put back into the specified array.
    *
    * @param array Array of values to swap
    */
  def swap(array: Array[Long]): Array[Long] = {
    for (foo <- array.indices) {
      array(foo) = swap(array(foo))
    }
    array
  }

  /**
    * Byte swap an array of floats. The result of the swapping
    * is put back into the specified array.
    *
    * @param array Array of values to swap
    */
  def swap(array: Array[Float]): Array[Float] = {
    for (foo <- array.indices) {
      array(foo) = swap(array(foo))
    }
    array
  }

  /**
    * Byte swap an array of doubles. The result of the swapping
    * is put back into the specified array.
    *
    * @param array Array of values to swap
    */
  def swap(array: Array[Double]): Array[Double] = {

    for (foo <- array.indices) {
      array(foo) = swap(array(foo))
    }
    array
  }
}





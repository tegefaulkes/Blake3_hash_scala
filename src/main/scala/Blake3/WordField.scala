package Blake3

case class WordField(data: String){

  def sizeInBytes: Int = data.length

  def sizeInBits: Int = sizeInBytes * 8

  def getSubBytes(beginIndex: Int, endIndex: Int): WordField = {
    val length = data.length
    WordField(data.substring(length - beginIndex, length - endIndex))
  }

  def subdivide(chunkSize: Int): Array[WordField] = {
    if(chunkSize >= sizeInBytes){
      Array(this)
    }else{
      val numberOfChunks = sizeInBytes / chunkSize
      val remainder = (sizeInBytes % chunkSize) > 0
      val arraySize = if(remainder) numberOfChunks + 1 else numberOfChunks
      val outputArray = new Array[WordField](arraySize)
      for(x <- 0 until numberOfChunks){
        val chunk = data.substring(x * chunkSize, (x + 1) * chunkSize)
        outputArray(x) = new WordField(chunk)
      }
      if(remainder){
        outputArray(numberOfChunks) = new WordField(data.substring(numberOfChunks * chunkSize))
      }
      outputArray
    }
  }

  def generateWord32Array: Array[Int] = {
    val chunkArray = subdivide(4)
    val arraySize = chunkArray.length
    val wordArray = new Array[Int](arraySize)
    for (x <- 0 until arraySize) {
      wordArray(x) = read32Word(chunkArray(x).toString)
    }
    wordArray
  }

  def generateWord64Array: Array[Long] = {
    val chunkArray = subdivide(8)
    val arraySize = chunkArray.length
    val wordArray = new Array[Long](arraySize)
    for (x <- 0 until arraySize) {
      wordArray(x) = read64Word(chunkArray(x).toString)
    }
    wordArray
  }

  def cat(that: WordField):WordField = {
    WordField(data.concat(that.data))
  }



  private def read32Word(stringWord: String): Int = {
    var word: Int = 0
    val byteArray = stringWord.getBytes()
    for (i <- 0 until 4){
      word += byteArray(i).toInt << (i * 8)
    }
    word
  }

  private def read64Word(stringWord: String): Long = {
    var word:Long = 0
    val byteArray = stringWord.getBytes()
    for (i <- 0 until 8){
      word += byteArray(i).toLong << (i * 8)
    }
    word
  }

  override def toString: String = data
}

object WordField{
  def zeros(length: Int): WordField = {
    val byteArray: Array[Byte] = Array.fill(length){0}
    WordField(new String(byteArray))
  }
}

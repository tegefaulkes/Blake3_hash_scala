package Blake3
//https://www.wikiwand.com/en/BLAKE_(hash_function)
//https://tools.ietf.org/pdf/rfc7693.pdf
class Blake2(in_M: WordField, in_Key: WordField, in_cbHashLen: Int, config: Bconf) {

  private var message = in_M
  private val cbMessageLen = in_M.sizeInBytes
  private val key = in_Key
  private val cbKeyLen = in_Key.sizeInBytes
  private val cbHashLen = if(in_cbHashLen > config.maxHashBytes) config.maxHashBytes else in_cbHashLen

  private var h = new Array[Bitfield](8)
  private var cBytesCompressed = Bitfield(0, 128)
  private var cBytesRemaining = cbMessageLen

  private val (iv, sigma) = config.getConstants
  private val (r1: Int, r2: Int, r3: Int, r4: Int) = config.gRConstants
  private val bb = config.blockBytes


  def pad(key: WordField, size: Int):WordField = {
    val keyLen = key.sizeInBytes
    val pad = size - keyLen
    val padding = WordField.zeros(pad)
    key.cat(padding)
  }

  def hash(): Bitfield = {
    iv.copyToArray(h)

    var newconst = Bitfield("01010000")
    newconst = newconst.setSubBits(Bitfield(cbKeyLen, 8), 8)
    newconst = newconst.setSubBits(Bitfield(cbHashLen, 8), 0)
    h(0) = h(0)^newconst

    if (cbKeyLen > 0) {
      message = pad(key, bb).cat(message)
      cBytesRemaining = cBytesRemaining + bb
    }

    while(cBytesRemaining > bb){
      val chunk = message.getSubBytes(cBytesRemaining,cBytesRemaining - bb)
      cBytesCompressed += bb
      cBytesRemaining  -= bb
      h = compress(h, chunk, cBytesCompressed, isLastBlock = false)
    }

    var chunk = message.getSubBytes(cBytesRemaining, 0)
    cBytesCompressed += cBytesRemaining
    chunk = pad(chunk, bb)
    h = compress(h, chunk, cBytesCompressed, isLastBlock = true)

//    Result ← first cbHashLen bytes of little endian state vector h
    Bitfield.combine(h) & Bitfield.genMask(cbHashLen.toInt * 8)
  }

  def compress(h: Array[Bitfield], chunk: WordField, incBytesCompressed: Bitfield, isLastBlock: Boolean):Array[Bitfield] = {

    var v = new Array[Bitfield](16)
    h.copyToArray(v)
    for(x <- 0 until 8) v(x+8) = iv(x)

    val split = incBytesCompressed.subdivide(2)
    v(12) = v(12) ^ split(1)
    v(13) = v(13) ^ split(0)

    if (isLastBlock) v(14) = v(14) ^ Bitfield("FFFFFFFFFFFFFFFF")


    val m = if(config.wordSize == 64) Bitfield(chunk.generateWord64Array) else Bitfield(chunk.generateWord32Array)


    for (i <- 0 until config.fRounds){
      val s = sigma(i%10)

      v = mix(v, 0, 4, 8,  12, m(s(0)), m(s(1)))
      v = mix(v, 1, 5, 9,  13, m(s(2)), m(s(3)))
      v = mix(v, 2, 6, 10, 14, m(s(4)), m(s(5)))
      v = mix(v, 3, 7, 11, 15, m(s(6)), m(s(7)))

      v = mix(v, 0, 5, 10, 15, m(s(8)), m(s(9)))
      v = mix(v, 1, 6, 11, 12, m(s(10)),m(s(11)))
      v = mix(v, 2, 7, 8,  13, m(s(12)),m(s(13)))
      v = mix(v, 3, 4, 9,  14, m(s(14)),m(s(15)))
    }
    for (x <- 0 until 8){
      h(x) = h(x) ^ v(x) ^ v(x+8)
    }
    h
  }

  def mix(inV: Array[Bitfield],a: Int, b: Int, c: Int, d: Int, x: Bitfield, y:Bitfield): Array[Bitfield] = {
    val v = new Array[Bitfield](16)
    inV.copyToArray(v)

    v(a) = v(a) + v(b) + x
    v(d) = (v(d) ^ v(a)).rotateright(r1)
    v(c) = v(c) + v(d)
    v(b) = (v(b) ^ v(c)).rotateright(r2)
    v(a) = v(a) + v(b) + y
    v(d) = (v(d) ^ v(a)).rotateright(r3)
    v(c) = v(c) + v(d)
    v(b) = (v(b) ^ v(c)).rotateright(r4)

    v
  }

}

object Blake2{

  def apply(in_M: WordField, in_Key: WordField, in_cbHashLen: Int, config: Bconf): String ={
    val blake = new Blake2(in_M, in_Key, in_cbHashLen, config)
    val hashValue = blake.hash()
    hashValue.reverseBytes.HexValue
  }

  def apply(in_M: WordField): String = {
    apply(in_M, WordField(""),64, Cblake2b_512_default())
  }

  def apply(in_M: String): String = {
    apply(WordField(in_M))
  }

  def b2b_512(in_M: WordField, in_Key: WordField): String = {
    Blake2(in_M, in_Key, 64,  Cblake2b_512_default())
  }

  def b2b_512(message: String, key: String): String = {
    Blake2(WordField(message), WordField(key), 64, Cblake2b_512_default())
  }

  def b2b_512(message: String): String = {
    b2b_512(message, "")
  }

  def b2s_256(in_m:WordField, in_key: WordField): String = {
    Blake2(in_m, in_key, 32, Cblake2s_256_default())
  }

  def b2s_256(message: String, key: String): String = {
    b2s_256(WordField(message), WordField(key))
  }

  def b2s_256(message: String): String = {
    b2s_256(message, "")
  }

}

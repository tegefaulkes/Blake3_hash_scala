package Blake3
//https://www.wikiwand.com/en/BLAKE_(hash_function)
class Blake2(in_M: Bitfield, in_cbMessageLen: Bitfield, in_Key: Bitfield, in_cbKeyLen: Bitfield, in_cbHashLen: Bitfield) {

  private var message = in_M
  private val cbMessageLen = in_cbMessageLen
  private val key = in_Key
  private val cbKeyLen = in_cbKeyLen
  private val cbHashLen = in_cbHashLen

  private var h = new Array[Bitfield](8)
  private var cBytesCompressed = Bitfield(0, 128)
  private var cBytesRemaining = cbMessageLen

  private val iv = new Array[Bitfield](8)
  iv(0) = Bitfield("6a09e667f3bcc908")
  iv(1) = Bitfield("bb67ae8584caa73b")
  iv(2) = Bitfield("3c6ef372fe94f82b")
  iv(3) = Bitfield("a54ff53a5f1d36f1")
  iv(4) = Bitfield("510e527fade682d1")
  iv(5) = Bitfield("9b05688c2b3e6c1f")
  iv(6) = Bitfield("1f83d9abfb41bd6b")
  iv(7) = Bitfield("5be0cd19137e2179")

  val sigma = new Array[Array[Int]](10)
  sigma(0) = Array(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  sigma(1) = Array(14,10,4,8,9,15,13,6,1,12,0,2,11,7,5,3)
  sigma(2) = Array(11,8,12,0,5,2,15,13,10,14,3,6,7,1,9,4)
  sigma(3) = Array(7,9,3,1,13,12,11,14,2,6,5,10,4,0,15,8)
  sigma(4) = Array(9,0,5,7,2,4,10,15,14,1,11,12,6,8,3,13)
  sigma(5) = Array(2,12,6,10,0,11,8,3,4,13,7,5,15,14,1,9)
  sigma(6) = Array(12,5,1,15,14,13,4,10,0,7,6,3,9,2,8,11)
  sigma(7) = Array(13,11,7,14,12,1,3,9,5,0,15,4,8,6,2,10)
  sigma(8) = Array(6,15,14,9,11,3,0,8,12,2,13,7,1,4,10,5)
  sigma(9) = Array(10,2,8,4,7,6,1,5,15,11,9,14,3,12,13,0)

  def pad(key: Bitfield, size: Int):Bitfield = {
    val keyLen = key.bitwidth
    val fieldB = Bitfield(size*8)
    val shift = size * 8 - keyLen
    val shiftedKey = (fieldB + key) << shift
    shiftedKey
  }

  def hash(): Bitfield = {
    iv.copyToArray(h)

    var newconst = Bitfield("01010000")
    newconst = newconst.setSubBits(Bitfield(cbKeyLen, 8), 8)
    newconst = newconst.setSubBits(Bitfield(cbHashLen, 8), 8)
    h(0) = h(0)^newconst

    if (cbKeyLen > 0) {
      message = pad(key, 128).cat(message)
      cBytesRemaining = cBytesRemaining + 128
    }

    while(cBytesRemaining > 128){
      val chunk = message.getSubBits(cBytesRemaining, cBytesRemaining-128)
      cBytesCompressed += 128
      cBytesRemaining  = cBytesRemaining - 128
      h = compress(h, chunk, cBytesCompressed, false)
    }

    var chunk = message.getSubBits(cBytesRemaining.toInt,0)
    cBytesCompressed += cBytesRemaining
    chunk = pad(chunk, 128)

    h = compress(h, chunk, cBytesCompressed, true)

//    Result ← first cbHashLen bytes of little endian state vector h
    Bitfield.combine(h) & Bitfield.genMask(cbHashLen.toInt) //TODO, combine is likely the wrong order.
  }

  def compress(h: Array[Bitfield], chunk: Bitfield, incBytesCompressed: Bitfield, isLastBlock: Boolean):Array[Bitfield] = {

    val v = new Array[Bitfield](16)
    h.copyToArray(v)
    for(x <- 0 until 8){
      v(x+8) = iv(x)
    }
    val split = incBytesCompressed.subdivide(2)
    v(12) = v(12) ^ split(0)
    v(13) = v(13) ^ split(1)

    if (isLastBlock){
      v(14) = v(14) ^ Bitfield("FFFFFFFFFFFFFFFF")
    }

    val m = chunk.subdivide(16)

    for (i <- 0 until 12){
      //        Select message mixing schedule for this round.
      //        BLAKE2b uses 12 rounds, while SIGMA has only 10 entries.
      //        S0..15 ← SIGMA[i mod 10]   Rounds 10 and 11 use SIGMA[0] and SIGMA[1] respectively
      val s = sigma(i%10)


      val a = mix(v(0), v(4), v(8), v(12), m(s(0)), m(s(1)))
      v(0) = a._1; v(4) = a._2; v(8) = a._3; v(12) = a._4
      val b = mix(v(1), v(5), v(9), v(13), m(s(2)), m(s(3)))
      v(1) = b._1; v(5) = b._2; v(9) = b._3; v(13) = b._4
      val c= mix(v(2), v(6), v(10),v(14), m(s(4)), m(s(5)))
      v(2) = c._1; v(6) = c._2; v(10) = c._3; v(14) = c._4
      val d = mix(v(3), v(7), v(11),v(15), m(s(6)), m(s(7)))
      v(3) = d._1; v(7) = d._2; v(11) = d._3; v(15) = d._4

      val e = mix(v(0), v(5), v(10),v(15), m(s(8)), m(s(9)))
      v(0) = e._1; v(5) = e._2; v(10) = e._3; v(15) = e._4
      val f = mix(v(1), v(6), v(11),v(12), m(s(10)),m(s(11)))
      v(1) = f._1; v(6) = f._2; v(11) = f._3; v(12) = f._4
      val g = mix(v(2), v(7), v(8), v(13), m(s(12)),m(s(13)))
      v(2) = g._1; v(7) = g._2; v(8) = g._3; v(13) = g._4
      val j = mix(v(3), v(4), v(9), v(14), m(s(14)),m(s(15)))
      v(3) = j._1; v(4) = j._2; v(9) = j._3; v(14) = j._4
    }
    for (x <- 0 until 8){
      h(x) = h(x) ^ v(x)
      h(x) = h(x) ^ v(x+8)
    }
    h
  }

  def mix(ia: Bitfield, ib: Bitfield, ic: Bitfield, id: Bitfield, ix: Bitfield, iy:Bitfield): (Bitfield, Bitfield, Bitfield, Bitfield ) = {
    var va = ia
    var vb = ib
    var vc = ic
    var vd = id
    val x = ix
    val y = iy

    va = va + vb + x
    vd = (vd ^ va).rotateright(32)
    vc = vc + vd
    vb = (vb ^ vc).rotateright(24)
    va = va + vb + y
    vd = (vd ^ va).rotateright(16)
    vc = vc + vd
    vb = (vb ^ vc).rotateright(63)

    (va, vb, vc, vd)
  }

}

object Blake2{

  def apply(in_M: Bitfield, in_cbMessageLen: Bitfield, in_Key: Bitfield, in_cbKeyLen: Bitfield, in_cbHashLen: Bitfield): String ={
    val blake = new Blake2(in_M, in_cbMessageLen, in_Key, in_cbKeyLen, in_cbHashLen)
    blake.hash().HexValue
  }

  def apply(in_M: Bitfield): String = {
    apply(in_M, Bitfield(in_M.bitwidth, 128), Bitfield(""), Bitfield(""),Bitfield("200"))
  }

  def apply(in_M: String): String = {
    apply(Bitfield(in_M))
  }
}

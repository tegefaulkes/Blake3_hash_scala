package Blake3

import scala.runtime.BoxesRunTime

case class Bconf( wordSize: Int,
                  fRounds: Int,
                  blockBytes: Int,
                  maxHashBytes: Int,
                  maxKeyBytes: Int,
                  maxInputBytes: Int,
                  gRConstants: (Int, Int, Int, Int),
                  constantSet: Int){

  private val iv = {
    val outIV = new Array[Bitfield](8)
    this.constantSet match {
      case 0 =>
        outIV(0) = Bitfield("6a09e667f3bcc908")
        outIV(1) = Bitfield("bb67ae8584caa73b")
        outIV(2) = Bitfield("3c6ef372fe94f82b")
        outIV(3) = Bitfield("a54ff53a5f1d36f1")
        outIV(4) = Bitfield("510e527fade682d1")
        outIV(5) = Bitfield("9b05688c2b3e6c1f")
        outIV(6) = Bitfield("1f83d9abfb41bd6b")
        outIV(7) = Bitfield("5be0cd19137e2179")
        outIV

      case 1 =>
        outIV(0) = Bitfield("6A09E667")
        outIV(1) = Bitfield("BB67AE85")
        outIV(2) = Bitfield("3C6EF372")
        outIV(3) = Bitfield("A54FF53A")
        outIV(4) = Bitfield("510E527F")
        outIV(5) = Bitfield("9B05688C")
        outIV(6) = Bitfield("1F83D9AB")
        outIV(7) = Bitfield("5BE0CD19")
        outIV
    }
  }

  private val sigma = {
    val sig = new Array[Array[Int]](10)
    sig(0) = Array(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
    sig(1) = Array(14,10,4,8,9,15,13,6,1,12,0,2,11,7,5,3)
    sig(2) = Array(11,8,12,0,5,2,15,13,10,14,3,6,7,1,9,4)
    sig(3) = Array(7,9,3,1,13,12,11,14,2,6,5,10,4,0,15,8)
    sig(4) = Array(9,0,5,7,2,4,10,15,14,1,11,12,6,8,3,13)
    sig(5) = Array(2,12,6,10,0,11,8,3,4,13,7,5,15,14,1,9)
    sig(6) = Array(12,5,1,15,14,13,4,10,0,7,6,3,9,2,8,11)
    sig(7) = Array(13,11,7,14,12,1,3,9,5,0,15,4,8,6,2,10)
    sig(8) = Array(6,15,14,9,11,3,0,8,12,2,13,7,1,4,10,5)
    sig(9) = Array(10,2,8,4,7,6,1,5,15,11,9,14,3,12,13,0)
    sig
  }

  def getConstants: (Array[Bitfield], Array[Array[Int]]) = {
    (iv, sigma)
  }
}

object Cblake2b{def apply(hashByte: Int, keyBytes: Int):Bconf = Bconf(64, 12,128, hashByte, keyBytes, 128, (32, 24, 16, 63), 0)}
object Cblake2s{def apply(hashByte: Int, keyBytes: Int):Bconf = Bconf(32, 10,64, hashByte, keyBytes, 128, (16, 12, 8, 7), 1)}
object Cblake2b_512_default{def apply(): Bconf = Cblake2b(64,0)}
object Cblake2s_256_default{def apply(): Bconf = Cblake2s(32, 0)}
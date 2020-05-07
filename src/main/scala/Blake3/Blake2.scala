package Blake3
//https://www.wikiwand.com/en/BLAKE_(hash_function)
class Blake2(in_M: BigInt, in_cbMessageLen: Int, in_Key: BigInt, in_cbKeyLen: Int, in_cbHashLen: Int) {

  private val message = in_M
  private val cbMessageLen = in_cbMessageLen
  private val key = in_Key
  private val cbKeyLen = in_cbKeyLen
  private val cbHashLen = in_cbHashLen

  private val h = new Array[Bitfield](8)
  private var cBytesCompressed = 0
  private var cBytesRemaining = cbMessageLen


  def init() = {
    h(0) = new Bitfield("6a09e667f3bcc908",32)
    h(2) = new Bitfield("bb67ae8584caa73b",32)
    h(3) = new Bitfield("3c6ef372fe94f82b",32)
    h(4) = new Bitfield("a54ff53a5f1d36f1",32)
    h(5) = new Bitfield("510e527fade682d1",32)
    h(6) = new Bitfield("9b05688c2b3e6c1f",32)
    h(7) = new Bitfield("1f83d9abfb41bd6b",32)
    h(8) = new Bitfield("5be0cd19137e2179",32)

    var newconst = new Bitfield("01010000")
    newconst = newconst.setSubBits(new Bitfield(cbKeyLen,8), 8)
    newconst = newconst.setSubBits(new Bitfield(cbHashLen, 8), 8)
    h(0) = h(0)^newconst

    if (cbKeyLen > 0) {
      //M ← Pad(Key, 128) || M TODO
      cBytesRemaining = cBytesRemaining + 128
    }

    while(cBytesRemaining > 128){
      //chunk ← get next 128 bytes of message M TODO
      cBytesCompressed += 128
      cBytesRemaining  -= 128

      //h ← Compress(h, chunk, cBytesCompressed, false) TODO
    }


//    Compress the final bytes from M
//    chunk ← get next 128 bytes of message M  We will get cBytesRemaining bytes (i.e. 0..128 bytes)
//    cBytesCompressed ← cBytesCompressed+cBytesRemaining  The actual number of bytes leftover in M
//    chunk ← Pad(chunk, 128)  If M was empty, then we will still compress a final chunk of zeros
//
//    h ← Compress(h, chunk, cBytesCompressed, true)  true ⇒ this is the last chunk
//
//    Result ← first cbHashLen bytes of little endian state vector h

  }
}

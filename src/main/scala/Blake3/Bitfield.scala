package Blake3

class Bitfield(value: BigInt, val width: Int) {
  val state: BigInt = value & genMask(width)
  val bitwidth = width

  def this(stringval: String, width: Int) {
    this(BigInt(stringval, 16), width)
  }
  def this(stringval: String){
    this(BigInt(stringval, 16), stringval.length()*4)
  }
  def this(value: Int, width: Int){
    this(BigInt(value), width)
  }


  def copy():Bitfield = {
    new Bitfield(state, bitwidth)
  }
  def resize(newWidth: Int):Bitfield = {
    new Bitfield(state, newWidth)
  }

  def getHexValue():String = {
    val stringLength = state.toString(16).length()
    val expectedLength = bitwidth/4
    val difference = expectedLength - stringLength
    "0" * difference + state.toString(16)
  }

  /**
   * generates a mask of all 1 bits of set width.
   * @param width width of mask in bits.
   * @return mask
   */
  private def genMask(width: Int): BigInt = {
    BigInt(2).pow(width) - 1
  }

  def getSubBits(posA: Int, posB: Int): Bitfield = {
    val width = posA - posB + 1
    val mask = genMask(width)
    val shift = if (posA < posB) posA else posB
    val returnvalue = (state & (mask << shift)) >> shift
    new Bitfield(returnvalue, width)
  }
  def getSubBits(pos: Int):Bitfield = {
    getSubBits(pos, pos)
  }

  def setSubBits(value: Bitfield, pos: Int): Bitfield = {
    val valueWidth = value.bitwidth
    val antiMask = genMask(bitwidth) ^ (genMask(valueWidth) << pos)
    val newvalue = (state & antiMask) + (value.state << pos)
    new Bitfield(newvalue, bitwidth)
  }

  def getSubChunk(bitWidth: Int, chunkPosition: Int, chunks: Int):Bitfield = {
    val mask = genMask(bitWidth)
    val shift = (chunks - chunkPosition - 1) * bitWidth
    val output = (state & (mask << shift)) >> shift
    new Bitfield(output, bitWidth)
  }

  def subdivide(divisions: Int):Array[Bitfield] = {
    if(bitwidth % divisions == 0){
      val output = new Array[Bitfield](divisions)
      val subWidth = bitwidth/divisions
      for (x <- 0 to divisions - 1){
        output(x) = getSubChunk(subWidth, x, divisions)
      }
      output
    }else{
      throw new RuntimeException(s"bidfield.subdivide({$width}) does not evenly divide {$bitwidth}")
    }
  }

  def cat(value: Bitfield):Bitfield = {
    val leftState = state
    val rightState = value.state
    val shift = value.bitwidth
    val newstate = (leftState << shift) + rightState
    new Bitfield(newstate, bitwidth + shift)
  }


  //infix operators.

  def +(that: Bitfield): Bitfield = {
    new Bitfield(state + that.state, bitwidth)
  }
  def +(that: Int):Bitfield = {
    new Bitfield(state + that, bitwidth)
  }
  //  def -(that: blake3.Bitfield): blake3.Bitfield = { //TODO finish this. look up 2's complement subtraction.
  //    val thisValue = state
  //    val thatValue = that.state
  //    val newValue = thisValue - thatValue
  //  }
  def *(that: Bitfield): Bitfield = {
    new Bitfield(state * that.state, bitwidth)
  }
  def *(that: Int): Bitfield = {
    new Bitfield(state * that, bitwidth)
  }
  def /(that: Bitfield): Bitfield = {
    new Bitfield(state / that.state, bitwidth)
  }

  def <<(shift: Int): Bitfield = {
    new Bitfield(state << shift, bitwidth)
  }
  def >>(shift: Int): Bitfield = {
    new Bitfield(state >> shift, bitwidth)
  }

  //bitwise
  def unary_~(): Bitfield ={
    new Bitfield(~state, bitwidth)
  }
  def &(that: Bitfield): Bitfield = {
    new Bitfield(state & that.state, bitwidth)
  }
  def |(that: Bitfield): Bitfield = {
    new Bitfield(state | that.state, bitwidth)
  }
  def ^(that: Bitfield): Bitfield = {
    new Bitfield(state ^ that.state, bitwidth)
  }

  //boolean
  def >(that: Bitfield): Boolean = {
    state > that.state
  }
  def >=(that: Bitfield): Boolean = {
    state >= that.state
  }
  def <(that: Bitfield): Boolean = {
    state < that.state
  }
  def <=(that: Bitfield): Boolean = {
    state <= that.state
  }
  def ==(that: Bitfield): Boolean = {
    state == that.state
  }
  def !=(that: Bitfield): Boolean = {
    state != that.state
  }


  def unary_+(): Bitfield ={
    new Bitfield(state, bitwidth)
  }

}



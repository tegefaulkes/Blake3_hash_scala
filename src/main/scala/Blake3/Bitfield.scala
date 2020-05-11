package Blake3

case class Bitfield(value: BigInt, bitwidth: Int) {
  val state: BigInt = {
    val maskedval = value & genMask(bitwidth)
    if(maskedval >=0 ) maskedval else BigInt(0)
  }

  def toInt:Int = {
    state.toInt
  }
  def toBigInt:BigInt = state

  def copy():Bitfield = {
    Bitfield(state, bitwidth)
  }
  def resize(newWidth: Int):Bitfield = {
    Bitfield(state, newWidth)
  }

  def HexValue:String = {
    val stringLength = state.toString(16).length()
    val expectedLength = bitwidth/4
    val difference = expectedLength - stringLength
    "0" * difference + state.toString(16)
  }

  def toByteArray: Array[Byte] = {
    value.toByteArray
  }

  def reverseBytes: Bitfield = {
    Bitfield(BigInt(toByteArray.reverse) >> 8, bitwidth)
  }

  private def genMask(width: Int): BigInt = {
    BigInt(2).pow(width) - 1
  }

  def getSubBits(posA: Int, posB: Int): Bitfield = {
    val width = posA - posB + 1
    val mask = genMask(width)
    val shift = if (posA < posB) posA else posB
    val returnvalue = (state & (mask << shift)) >> shift
    Bitfield(returnvalue, width)
  }
  def getSubBits(pos: Int):Bitfield = {
    getSubBits(pos, pos)
  }
  def getSubBits(posA: Bitfield, posB: Bitfield):Bitfield = {
    getSubBits(posA.toInt, posB.toInt)
  }
  def getSubBits(pos: Bitfield):Bitfield = {
    getSubBits(pos,pos)
  }

  def setSubBits(value: Bitfield, pos: Int): Bitfield = {
    val valueWidth = value.bitwidth
    val antiMask = genMask(bitwidth) ^ (genMask(valueWidth) << pos)
    val newvalue = (state & antiMask) + (value.state << pos)
    Bitfield(newvalue, bitwidth)
  }

  def getSubChunk(width: Int, chunkPosition: Int, chunks: Int):Bitfield = {
    val mask = genMask(width)
    val shift = (chunks - chunkPosition - 1) * width
    val output = (state & (mask << shift)) >> shift
    Bitfield(output, width)
  }

  def subdivide(divisions: Int):Array[Bitfield] = {
    if(bitwidth % divisions == 0){
      val output = new Array[Bitfield](divisions)
      val subWidth = bitwidth/divisions
      for (x <- 0 until divisions){
        output(x) = getSubChunk(subWidth, x, divisions)
      }
      output
    }else{
      throw new RuntimeException(s"bidfield.subdivide({$divisions}) does not evenly divide {$bitwidth}")
    }
  }

  def rotateright(shift: Int): Bitfield = {
    val newshift = shift % bitwidth
    val mask = genMask(newshift)
    val maskedValue = state & mask
    val shifteda  = state >> newshift
    val shiftedb = maskedValue << (bitwidth - newshift)
    Bitfield(shifteda+shiftedb, bitwidth)
  }

  def cat(value: Bitfield):Bitfield = {
    val leftState = state
    val rightState = value.state
    val shift = value.bitwidth
    val newstate = (leftState << shift) + rightState
    Bitfield(newstate, bitwidth + shift)
  }


  //infix operators.

  def +(that: Bitfield): Bitfield = {
    Bitfield(state + that.state, bitwidth)
  }
  def +(that: Int):Bitfield = {
    Bitfield(state + that, bitwidth)
  }
  def -(that: Bitfield): Bitfield = {
    Bitfield(state - that.state, bitwidth)
  }
  def -(that: Int): Bitfield = {
    val newValue = state - that
    Bitfield(state - that, bitwidth)
  }
  def *(that: Bitfield): Bitfield = {
    Bitfield(state * that.state, bitwidth)
  }
  def *(that: Int): Bitfield = {
    Bitfield(state * that, bitwidth)
  }
  def /(that: Bitfield): Bitfield = {
    Bitfield(state / that.state, bitwidth)
  }

  def <<(shift: Int): Bitfield = {
    Bitfield(state << shift, bitwidth)
  }
  def >>(shift: Int): Bitfield = {
    Bitfield(state >> shift, bitwidth)
  }

  //bitwise
  def unary_~(): Bitfield ={
    Bitfield(~state, bitwidth)
  }
  def &(that: Bitfield): Bitfield = {
    Bitfield(state & that.state, bitwidth)
  }
  def |(that: Bitfield): Bitfield = {
    Bitfield(state | that.state, bitwidth)
  }
  def ^(that: Bitfield): Bitfield = {
    Bitfield(state ^ that.state, bitwidth)
  }

  //boolean
  def >(that: Bitfield): Boolean = {
    state > that.state
  }
  def >(that: Int): Boolean = {
    state > that
  }
  def >=(that: Bitfield): Boolean = {
    state >= that.state
  }
  def >=(that: Int): Boolean = {
    state >= that
  }
  def < (that: Bitfield): Boolean = {
    state < that.state
  }
  def < (that: Int): Boolean = {
    state < that
  }
  def <=(that: Bitfield): Boolean = {
    state <= that.state
  }
  def <=(that: Int): Boolean = {
    state <= that
  }
  def ==(that: Bitfield): Boolean = {
    state == that.state
  }
  def ==(that: Int): Boolean = {
    state == that
  }
  def !=(that: Bitfield): Boolean = {
    state != that.state
  }
  def !=(that: Int): Boolean = {
    state != that
  }

  def unary_+(): Bitfield ={
    Bitfield(state, bitwidth)
  }

}

object Bitfield {
  def apply(stringval: String, width: Int):Bitfield = {
    Bitfield(BigInt(stringval, 16), width)
  }

  def apply(stringval: String):Bitfield = {
    if(stringval.length() == 0) Bitfield(BigInt(0),0)
    else Bitfield(BigInt(stringval, 16), stringval.length()*4)
  }

  def apply(value: Int, width: Int): Bitfield = {
    Bitfield(BigInt(value), width)
  }

  def apply(width: Int): Bitfield = {
    Bitfield(BigInt(0), width)
  }

  def apply(value: Bitfield, width: Int): Bitfield = {
    Bitfield(value.state, width)
  }

  def apply(value: Bitfield): Bitfield = {
    Bitfield(value.state, value.bitwidth)
  }

  def apply(longArray: Array[Long]):Array[Bitfield] = {
    val length = longArray.length
    val fieldArray = new Array[Bitfield](length)
    for(x <- 0 until length){
      fieldArray(x) = Bitfield(BigInt(longArray(x)), 64)
    }
    fieldArray
  }

  def combine(in: Array[Bitfield]):Bitfield = {
    var totalBits = 0
    var totalValue = BigInt(0)
    for(field <- in){
      totalValue += field.state << totalBits
      totalBits += field.bitwidth
    }
    Bitfield(totalValue, totalBits)
  }

  def genMask(width: Int): Bitfield = {
    Bitfield(BigInt(2).pow(width) - 1, width)
  }

  def genMask(width: Bitfield): Bitfield = {
    genMask(width.toInt)
  }
}



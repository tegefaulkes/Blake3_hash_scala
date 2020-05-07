package Blake3

class Blake3 {
  def hello()={
    println("hello 1")
  }
}

object Blake3{
  def main(args: Array[String])={
    val test = new Blake3
    test.hello()

//    val a = new Bitfield("12345678", 32)
//        val b = new Bitfield("0f", 8)
//        val c = a.subdivide(8)
//        for(x <- c){
//          println(x.resize(8).getHexValue())
//        }

    val a = new Bitfield("0000")
    val b = new Bitfield("A")
    val c = a.setSubBits(b,4)
    println(c.getHexValue())
  }
}

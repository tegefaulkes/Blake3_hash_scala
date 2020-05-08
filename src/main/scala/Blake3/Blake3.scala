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

    val a = Bitfield("1234")
    val b = Bitfield("A")
    val c = a.setSubBits(b,4)
    println(a.HexValue)
    println(a.rotateright(2).HexValue)

    println(Blake2(""))
    println("786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce")

  }
}

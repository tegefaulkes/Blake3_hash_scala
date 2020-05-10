package Blake3

object Blake3{
  def main(args: Array[String])={
    val test = WordField("thequickbrownfox!")

    for(x <- test.generateWord32Array){
      println(x)
    }

    for(x <- test.generateWord64Array){
      println(x)
    }

   println(Blake2.b2b_512(""))



//    println("Result: ", Blake2(""))
//    println("Should: ","786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce")

  }
}

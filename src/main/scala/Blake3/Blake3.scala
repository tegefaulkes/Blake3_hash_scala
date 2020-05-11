package Blake3

object Blake3{
  def main(args: Array[String])={


    def check(message: String, expected: String): Unit ={
      val length = expected.length
      val result = Blake2.b2b_512(message)
      var difference = ""
      for(x <- 0 until length){
        if(result(x) == expected(x)){
          difference += "-"
        }else {
          difference += "H"//difference += BigInt(result(x), 16) ^ BigInt(expected(x), 16)
        }
      }
      println(difference)
    }

//    check("","786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce")
    check("The quick brown fox jumps over the lazy dog", "a8add4bdddfd93e4877d2746e62817b116364a1fa7bc148d95090bc7333b3673f82401cf7aa2e4cb1ecd90296e3f14cb5413f8ed77be73045b13914cdcd6a918")
//    check("The quick brown fox jumps over the lazy dof", "ab6b007747d8068c02e25a6008db8a77c218d94f3b40d2291a7dc8a62090a744c082ea27af01521a102e42f480a31e9844053f456b4b41e8aa78bbe5c12957bb")

//    println("Result: ", Blake2.b2b_512(""))
//    println("Should: ","786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce")
//    println("")
//    println("Result: ", Blake2.b2b_512("The quick brown fox jumps over the lazy dog"))
//    println("Should: ","a8add4bdddfd93e4877d2746e62817b116364a1fa7bc148d95090bc7333b3673f82401cf7aa2e4cb1ecd90296e3f14cb5413f8ed77be73045b13914cdcd6a918")
//    println("")
//    println("Result: ", Blake2.b2b_512("The quick brown fox jumps over the lazy dof"))
//    println("Should: ","ab6b007747d8068c02e25a6008db8a77c218d94f3b40d2291a7dc8a62090a744c082ea27af01521a102e42f480a31e9844053f456b4b41e8aa78bbe5c12957bb")
//    println("")

  }
}

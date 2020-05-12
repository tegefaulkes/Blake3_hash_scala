package Blake3

object Blake3{
  def main(args: Array[String]): Unit={


    def check(result: String, key: String,  expected: String): Unit ={
      val length = expected.length
      //val result = Blake2.b2b_512(message)
      var difference = ""
      for(x <- 0 until length){
        if(result(x) == expected(x)){
          difference += "-"
        }else {
          difference += "H"//difference += BigInt(result(x), 16) ^ BigInt(expected(x), 16)
        }
      }
      //println(result)
      //println(expected)
      println(difference)
//      println("")
    }

    def checkBlake2b512(message:String, expected:String): Unit = {
      check(Blake2.b2b_512(message), "", expected)
    }

    def checkBlake2s256(message:String, expected:String): Unit = {
      check(Blake2.b2s_256(message), "", expected)
    }


    println("Blake2b tests")
    checkBlake2b512("","786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce")
    checkBlake2b512("abc", "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d17d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923")
    checkBlake2b512("The quick brown fox jumps over the lazy dog", "a8add4bdddfd93e4877d2746e62817b116364a1fa7bc148d95090bc7333b3673f82401cf7aa2e4cb1ecd90296e3f14cb5413f8ed77be73045b13914cdcd6a918")
    checkBlake2b512("The quick brown fox jumps over the lazy dof", "ab6b007747d8068c02e25a6008db8a77c218d94f3b40d2291a7dc8a62090a744c082ea27af01521a102e42f480a31e9844053f456b4b41e8aa78bbe5c12957bb")
    checkBlake2b512("this is a test hash", "5c1c162c8b771c3a68ae76b022ecc2970c6dbde09a6f5f4dcdc8ab4c877ffd9773860761da0626fc34a86130433fe106f1bac2baef6738525e47abbc090982bd")
    checkBlake2b512("this is a test hasf", "f79bf36ffbe2ed95d138874812538f77b7c9bb7e70c927b7ecb23d41db8738dd9842b1011673355f6af1da926e4ec3433cde4bc84519c059dc839d3fdb24d8a9")
    checkBlake2b512("This is a very long message that makes sure that the multiple rounds of hashing happend so that we can test stuf safashdkfhaskdfhskflaskfdjhalskjhflsakjfdhaskljfdhlsakjfdhklsadjhfsakjdhfklsajdhflksajhfdlksajdhfklsajdfhklsajfdh", "24829a8943818d411ba4a13eea000e3ce580106e6b2f23dba9e57b451588300dd9e90b64dcd6cc479fa1972cd0ba7693b75c12b725bc2c549062b6e791f1200c")
    println("")
    println("blake2s tests")
    checkBlake2s256("","69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9")
    checkBlake2s256("abc","508c5e8c327c14e2e1a72ba34eeb452f37458b209ed63a294d999b4c86675982")
    checkBlake2s256("The quick brown fox jumps over the lazy dog","606beeec743ccbeff6cbcdf5d5302aa855c256c29b88c8ed331ea1a6bf3c8812")
    checkBlake2s256("The quick brown fox jumps over the lazy dof","ae8ce27c652988829d43a30e38a710e59c5adacab9076d8289d0f44976a567e8")
    checkBlake2s256("this is a test hash","c4d49533fa0c263426243694001fae9419c10b2460a73342f25ff9e1f1c753c5")
    checkBlake2s256("this is a test hasf","ea791136c76fe228a2a6c6a05700c739d32a3def3fa20b4ac4a45abd24c554b3")
    checkBlake2s256("This is a very long message that makes sure that the multiple rounds of hashing happend so that we can test stuf safashdkfhaskdfhskflaskfdjhalskjhflsakjfdhaskljfdhlsakjfdhklsadjhfsakjdhfklsajdhflksajhfdlksajdhfklsajdfhklsajfdh", "33cbba708e6b77282c53cd569a43c6e7277e3d19f5a678219b28ada2d45d881d")

  }
}

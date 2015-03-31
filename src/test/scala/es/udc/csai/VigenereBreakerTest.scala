package es.udc.csai

import es.udc.csai.Language.English
import org.scalatest.{Matchers, WordSpecLike}

class VigenereBreakerTest extends Matchers with WordSpecLike {
  "A VigenereBreaker object" should {
    "Return the list of most likely key lengths based on gap matches" in {
      VigenereBreaker.guessLength("ASDASD", 8) shouldBe Seq(3 -> 3)
      VigenereBreaker.guessLength("ASDAFD", 8) shouldBe Seq(3 -> 2)
      VigenereBreaker.guessLength("AADAFD", 8) shouldBe Seq(3 -> 2, 1 -> 1, 2 -> 1)
    }
    "Test all the keys " in {
      val text = "BE WATER AND THAT OF THE END ALSO THAT I THIS BE SAND THE OF LAND ME I JUST KEEP THIS THAT STORM CIPHER KEY BE HAVE"
      val init = System.currentTimeMillis()
      VigenereBreaker.decipherBruteForce(Vigenere.cipher(text, "ZY"), 8, 4, 512, (k, v) => v shouldBe text)
      println(s"Brute force time: ${System.currentTimeMillis() - init} ms")
    }
    "Decipher the text using text analysis" in {
      val text = "BE WATER AND THAT OF THE END ALSO THAT I THIS BE SAND THE OF LAND ME I JUST KEEP THIS THAT STORM CIPHER KEY BE HAVE"
      val init = System.currentTimeMillis()
      val ec = Vigenere.cipher(text, "ZYXV")
      VigenereBreaker.decipherPartitions(
        text = ec,
        lengths = VigenereBreaker.guessLength(text, 4).map(_._1),
        matches = 4,
        snippetLength = 64,
        numCharsTested = 8,
        output = (k, v) => v shouldBe text)
      println(s"Key guessing time: ${System.currentTimeMillis() - init} ms")
    }
  }
}
